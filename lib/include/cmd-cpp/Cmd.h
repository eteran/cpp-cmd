/*
Copyright (c) 2021-2021, Evan Teran <evan.teran at gmail dot com>
Copyright (c) 2010-2014, Salvatore Sanfilippo <antirez at gmail dot com>
Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef CMD_H_
#define CMD_H_

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <functional>
#include <map>
#include <optional>
#include <queue>
#include <string>
#include <string_view>
#include <vector>

#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

class Cmd {
private:
	static constexpr int MaxLineLength = 4096;

	using hints_type   = std::function<std::optional<std::string>(std::string_view, int *, bool *)>;
	using command_type = std::function<bool(std::string_view)>;

	// The State structure represents the state during line editing.
	// We pass this state to functions implementing specific editing functionalities.
	struct State {
		std::string buf;      // Edited line buffer.
		size_t pos;           // Current cursor position.
		size_t oldpos;        // Previous refresh cursor position.
		size_t cols;          // Number of columns in terminal.
		size_t maxrows;       // Maximum num of rows used so far (multiline mode)
		size_t history_index; // The history index we are currently editing.
	};

	struct ParseResult {
		std::string cmd;
		std::string args;
		std::string line;
	};

	enum class Direction {
		Next,
		Prev,
	};

	enum KEY_ACTION {
		KEY_NULL  = 0,  // NUL
		CTRL_A    = 1,  // Ctrl+a
		CTRL_B    = 2,  // Ctrl-b
		CTRL_C    = 3,  // Ctrl-c
		CTRL_D    = 4,  // Ctrl-d
		CTRL_E    = 5,  // Ctrl-e
		CTRL_F    = 6,  // Ctrl-f
		CTRL_H    = 8,  // Ctrl-h
		TAB       = 9,  // Tab
		LINE_FEED = 10, // Ctrl+Enter
		CTRL_K    = 11, // Ctrl+k
		CTRL_L    = 12, // Ctrl+l
		ENTER     = 13, // Enter
		CTRL_N    = 14, // Ctrl-n
		CTRL_P    = 16, // Ctrl-p
		CTRL_T    = 20, // Ctrl-t
		CTRL_U    = 21, // Ctrl+u
		CTRL_W    = 23, // Ctrl+w
		CTRL_Z    = 26,
		ESC       = 27, // Escape
		BACKSPACE = 127 // Backspace
	};

private:
	// trim from start (in place)
	static void ltrim(std::string &s) {
		s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) { return !std::isspace(ch); }));
	}

	// trim from end (in place)
	static void rtrim(std::string &s) {
		s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) { return !std::isspace(ch); }).base(), s.end());
	}

	// trim from both ends (in place)
	static void trim(std::string &s) {
		ltrim(s);
		rtrim(s);
	}

	static bool starts_with(std::string_view s, std::string_view prefix) {
		return std::mismatch(prefix.begin(), prefix.end(), s.begin()).first == prefix.end();
	}

	static bool isUnsupportedTerm() {

		const char *term = getenv("TERM");
		if (!term) {
			return false;
		}

		static const char *unsupported_terms[] = {"dumb", "cons25", "emacs"};
		for (const char *t : unsupported_terms) {
			if (!strcasecmp(term, t)) {
				return true;
			}
		}

		return false;
	}

	// Beep, used for completion when there is nothing to complete or when all the choices were already shown.
	static void beep() {
		fprintf(stderr, "\a");
		fflush(stderr);
	}

public:
	Cmd(const std::string &history_file, int complete_key = TAB, FILE *in = stdin, FILE *out = stdout)
		: in_(in), out_(out), complete_key_(complete_key), history_file_(history_file) {

		in_fd_  = fileno(in_);
		out_fd_ = fileno(out_);

		loadHistory();
	}

	Cmd(int complete_key = TAB, FILE *in = stdin, FILE *out = stdout)
		: in_(in), out_(out), complete_key_(complete_key) {

		in_fd_  = fileno(in_);
		out_fd_ = fileno(out_);
	}

	~Cmd() {
		// At exit we'll try to fix the terminal to the initial conditions.
		disableRawMode(in_fd_);

		saveHistory();
	}

public:
	// Register a hits function to be called to show hits to the user at the right of the prompt.
	void setHintsCallback(hints_type fn) {
		hints_handler_ = fn;
	}

	void registerCommand(const std::string &cmd, command_type handler) {
		commands_.emplace(cmd, handler);
	}

	// Clear the screen. Used to handle ctrl+l
	void clearScreen() {
		write_string("\x1b[H\x1b[2J");
	}

	void cmdLoop() {
		bool stop = false;

		while (!stop) {
			std::optional<std::string> line = nextLine();
			if (!line) {
				break;
			}
			stop = oneCmd(*line);
		}
	}

	void saveHistory() {
		if (history_file_) {
			std::ofstream file(*history_file_);
			for (const std::string &entry : history_) {
				file << entry << '\n';
			}
		}
	}

	void loadHistory() {
		if (history_file_) {
			std::ifstream file(*history_file_);
			for (std::string line; std::getline(file, line);) {
				addHistoryEntry(line);
			}
		}
	}

private:
	// This function is called when the standard input file descriptor not attached to a TTY.
	// So for example when the program is called in pipe or with a file redirected to its standard input.
	// In this case, we want to be able to return the line regardless of its length (by default we are limited to 4k).
	std::string readlineNoTTY() const {
		std::string line;

		while (true) {
			int ch = fgetc(in_);
			if (ch == EOF || ch == '\n') {
				return line;
			} else {
				line.push_back(ch);
			}
		}
	}

	std::optional<std::string> nextLine() {
		if (!isatty(in_fd_)) {
			// Not a tty: read from file / pipe. In this mode we don't want any limit to the line size, so we call a function to handle that.
			return readlineNoTTY();

		} else if (isUnsupportedTerm()) {

			write_string(prompt);
			fflush(out_);

			char buf[MaxLineLength];
			if (fgets(buf, sizeof(buf), in_) == nullptr) {
				return {};
			}

			size_t len = strlen(buf);
			while (len && (buf[len - 1] == '\n' || buf[len - 1] == '\r')) {
				len--;
				buf[len] = '\0';
			}

			return buf;
		} else {
			return readlineRaw();
		}
	}

	bool oneCmd(const std::string &str) {
		auto [cmd, arg, line] = parseline(str);

		if (line.empty()) {
			return emptyline();
		}

		if (cmd.empty()) {
			return defaultHandler(line);
		}

		addHistoryEntry(line);

		auto it = commands_.find(cmd);
		if (it == commands_.end()) {
			return defaultHandler(line);
		}

		auto func = it->second;
		return func(arg);
	}

	bool emptyline() {
		if (!history_.empty()) {
			return oneCmd(history_.back());
		}

		return false;
	}

	bool defaultHandler(const std::string &line) {
		write_string("*** Unknown command: ");
		write_string(line);
		write_char('\n');
		return false;
	}

	ParseResult parseline(std::string line) const {

		trim(line);

		if (line.empty()) {
			return {};
		}

		size_t i = 0;
		while (i < line.size() && identchars_.find(line[i]) != std::string::npos) {
			++i;
		}

		std::string cmd  = line.substr(0, i);
		std::string args = line.substr(i);
		trim(args);
		return ParseResult{cmd, args, line};
	}

	// This function calls the line editing function editLine() using the STDIN file descriptor set in raw mode.
	std::optional<std::string> readlineRaw() {

		int r = enableRawMode(in_fd_);
		if (r < 0) {
			return {};
		}

		std::optional<std::string> buf = editLine();
		disableRawMode(in_fd_);
		write_char('\n');
		return buf;
	}

	int enableRawMode(int fd) {

		if (raw_mode) {
			return 0;
		}

		if (!isatty(in_fd_)) {
			return -ENOTTY;
		}

		if (tcgetattr(fd, &orig_termios_) == -1) {
			return -ENOTTY;
		}

		struct termios raw = orig_termios_; // modify the original mode

		// input modes: no break, no CR to NL, no parity check, no strip char, no start/stop output control.
		raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
		// output modes - disable post processing
		raw.c_oflag &= ~(OPOST);
		// control modes - set 8 bit chars
		raw.c_cflag |= (CS8);
		// local modes - echoing off, canonical off, no extended functions, no signal chars (^Z,^C)
		raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
		// control chars - set return condition: min number of bytes and timer. We want read to return every single byte, without timeout.
		raw.c_cc[VMIN]  = 1;
		raw.c_cc[VTIME] = 0; // 1 byte, no timer

		// put terminal in raw mode after flushing
		if (tcsetattr(fd, TCSAFLUSH, &raw) < 0)
			return -ENOTTY;

		raw_mode = true;
		return 0;
	}

	void disableRawMode(int fd) {
		// Don't even check the return value as it's too late.
		if (raw_mode && tcsetattr(fd, TCSAFLUSH, &orig_termios_) != -1) {
			raw_mode = false;
		}
	}

	// This function is the core of the line editing capability.
	// The resulting string is put into 'buf' when the user type enter, or when ctrl+d is typed.
	// The function returns the length of the current buffer.
	std::optional<std::string> editLine() {
		State state;

		// Populate the state that we pass to functions implementing specific editing functionalities.
		state.oldpos        = 0;
		state.pos           = 0;
		state.cols          = getColumns();
		state.maxrows       = 0;
		state.history_index = 0;

		// The latest history entry is always our current buffer, that initially is just an empty string.
		addHistoryEntry("");

		if (write_string(prompt) == -1) {
			return {};
		}

		while (true) {
			char ch;
			char seq[3];

			int nread = read(in_fd_, &ch, 1);
			if (nread <= 0) {
				return state.buf;
			}

			// Only autocomplete when the callback is set. It returns < 0 when there was an error reading from fd.
			// Otherwise it will return the character that should be handled next.
			if (ch == complete_key_) {
				int r = completeLine(&state);
				// Return on errors
				if (ch < 0)
					return state.buf;

				// Read next character when 0
				if (ch == '\0')
					continue;

				ch = r;
			}

			switch (ch) {
			case LINE_FEED:
			case ENTER: // enter
				history_.pop_back();

				if (hints_handler_) {
					// Force a refresh without hints to leave the previous line as the user typed it after a newline.
					hints_type hc  = hints_handler_;
					hints_handler_ = nullptr;
					refreshLine(&state);
					hints_handler_ = hc;
				}
				return state.buf;
			case CTRL_C: // ctrl-c
				return {};
			case CTRL_Z: /* ctrl-z */
#ifdef SIGTSTP
				/* send ourselves SIGSUSP */
				disableRawMode(in_fd_);
				raise(SIGTSTP);
				/* and resume */
				enableRawMode(in_fd_);
				refreshLine(&state);
#endif
				continue;
			case BACKSPACE: // backspace
			case CTRL_H:    // ctrl-h
				editBackspace(&state);
				break;
			case CTRL_D: // ctrl-d, remove char at right of cursor, or if the line is empty, act as end-of-file.
				if (state.buf.size() > 0) {
					editDelete(&state);
				} else {
					history_.pop_back();
					return {};
				}
				break;
			case CTRL_T: // ctrl-t, swaps current character with previous.
				if (state.pos > 0 && state.pos < state.buf.size()) {
					std::swap(state.buf[state.pos], state.buf[state.pos - 1]);

					if (state.pos != state.buf.size() - 1)
						state.pos++;
					refreshLine(&state);
				}
				break;
			case CTRL_B: // ctrl-b
				editMoveLeft(&state);
				break;
			case CTRL_F: // ctrl-f
				editMoveRight(&state);
				break;
			case CTRL_P: // ctrl-p
				editHistoryNext(&state, Direction::Prev);
				break;
			case CTRL_N: // ctrl-n
				editHistoryNext(&state, Direction::Next);
				break;
			case ESC: // escape sequence
				// Read the next two bytes representing the escape sequence. Use two calls to handle slow terminals returning the two chars at different times.
				if (read(in_fd_, seq, 1) == -1)
					break;
				if (read(in_fd_, seq + 1, 1) == -1)
					break;

				// ESC [ sequences.
				if (seq[0] == '[') {
					if (isdigit(seq[1])) {
						// Extended escape, read additional byte.
						if (read(in_fd_, seq + 2, 1) == -1)
							break;
						if (seq[2] == '~') {
							switch (seq[1]) {
							case '3': // Delete key.
								editDelete(&state);
								break;
							}
						}
					} else {
						switch (seq[1]) {
						case 'A': // Up
							editHistoryNext(&state, Direction::Prev);
							break;
						case 'B': // Down
							editHistoryNext(&state, Direction::Next);
							break;
						case 'C': // Right
							editMoveRight(&state);
							break;
						case 'D': // Left
							editMoveLeft(&state);
							break;
						case 'H': // Home
							editMoveHome(&state);
							break;
						case 'F': // End
							editMoveEnd(&state);
							break;
						}
					}
				}

				// ESC O sequences.
				else if (seq[0] == 'O') {
					switch (seq[1]) {
					case 'H': // Home
						editMoveHome(&state);
						break;
					case 'F': // End
						editMoveEnd(&state);
						break;
					}
				}
				break;
			default:
				if (editInsert(&state, ch)) {
					return {};
				}
				break;
			case CTRL_U: // Ctrl+u, delete the whole line.
				state.buf.clear();
				state.pos = 0;
				refreshLine(&state);
				break;
			case CTRL_K: // Ctrl+k, delete from current to end of line.
				state.buf.erase(state.pos);
				refreshLine(&state);
				break;
			case CTRL_A: // Ctrl+a, go to the start of the line
				editMoveHome(&state);
				break;
			case CTRL_E: // ctrl+e, go to the end of the line
				editMoveEnd(&state);
				break;
			case CTRL_L: // ctrl+l, clear screen
				clearScreen();
				refreshLine(&state);
				break;
			case CTRL_W: // ctrl+w, delete previous word
				editDeletePreviousWord(&state);
				break;
			}
		}
		return state.buf;
	}

	// Insert the character 'c' at cursor current position. On error writing to the terminal -1 is returned, otherwise 0.
	int editInsert(State *state, char ch) {
		if (state->buf.size() == state->pos) {
			state->buf.insert(state->pos++, 1, ch);

			if (prompt.size() + state->buf.size() < state->cols && !hints_handler_) {
				// Avoid a full update of the line in the trivial case.
				const char d = (mask_mode) ? '*' : ch;
				if (write_char(d) == -1) {
					return -1;
				}
			} else {
				refreshLine(state);
			}
		} else {
			state->buf.insert(state->pos++, 1, ch);
			refreshLine(state);
		}
		return 0;
	}

	// Move cursor on the left.
	void editMoveLeft(State *state) {
		if (state->pos > 0) {
			state->pos--;
			refreshLine(state);
		}
	}

	// Move cursor on the right.
	void editMoveRight(State *state) {
		if (state->pos != state->buf.size()) {
			state->pos++;
			refreshLine(state);
		}
	}

	// Move cursor to the start of the line.
	void editMoveHome(State *state) {
		if (state->pos != 0) {
			state->pos = 0;
			refreshLine(state);
		}
	}

	// Move cursor to the end of the line.
	void editMoveEnd(State *state) {
		if (state->pos != state->buf.size()) {
			state->pos = state->buf.size();
			refreshLine(state);
		}
	}

	// Substitute the currently edited line with the next or previous history entry as specified by 'dir'.
	void editHistoryNext(State *state, Direction dir) {

		if (!history_.empty()) {
			// Update the current history entry before to overwrite it with the next one.
			history_[history_.size() - 1 - state->history_index] = state->buf;

			switch (dir) {
			case Direction::Prev:
				++state->history_index;
				if (state->history_index >= history_.size()) {
					state->history_index = history_.size() - 1;
					return;
				}
				break;
			case Direction::Next:
				if (state->history_index == 0) {
					return;
				}
				--state->history_index;
			}

			// Show the new entry
			state->buf = history_[history_.size() - 1 - state->history_index];
			state->pos = state->buf.size();
			refreshLine(state);
		}
	}

	// Delete the character at the right of the cursor without altering the cursor position.
	// Basically this is what happens with the "Delete" keyboard key.
	void editDelete(State *state) {
		if (!state->buf.empty() && state->pos < state->buf.size()) {
			state->buf.erase(state->pos, 1);
			refreshLine(state);
		}
	}

	// Backspace implementation.
	void editBackspace(State *state) {
		if (state->pos > 0 && !state->buf.empty()) {
			state->buf.erase(--state->pos, 1);
			refreshLine(state);
		}
	}

	// Delete the previous word, maintaining the cursor at the start of the current word.
	void editDeletePreviousWord(State *state) {
		const size_t old_pos = state->pos;

		while (state->pos > 0 && state->buf[state->pos - 1] == ' ')
			state->pos--;

		while (state->pos > 0 && state->buf[state->pos - 1] != ' ')
			state->pos--;

		const size_t diff = old_pos - state->pos;
		state->buf.erase(state->pos, diff);
		refreshLine(state);
	}

	void refreshLine(State *state) {
		refreshSingleLine(state);
	}

	// Single line low level line refresh. Rewrite the currently edited line accordingly to the buffer content, cursor position, and number of columns of the terminal.
	void refreshSingleLine(State *state) {
		char seq[64];
		const size_t plen = prompt.size();
		std::string_view buf_view(state->buf);
		size_t pos = state->pos;
		std::string ab;

		while (plen + pos >= state->cols) {
			buf_view.remove_prefix(1);
			pos--;
		}

		while (plen + buf_view.size() > state->cols) {
			buf_view.remove_suffix(1);
		}

		// Cursor to left edge
		snprintf(seq, sizeof(seq), "\r");
		ab.append(seq);
		// Write the prompt and the current buffer content
		ab.append(prompt);

		if (mask_mode) {
			ab.append(buf_view.size(), '*');
		} else {
			ab.append(buf_view);
		}
		// Show hits if any.
		refreshShowHints(ab, state, plen);
		// Erase to right
		snprintf(seq, sizeof(seq), "\x1b[0K");
		ab.append(seq);
		// Move cursor to original position.
		snprintf(seq, sizeof(seq), "\r\x1b[%dC", static_cast<int>(pos + plen));
		ab.append(seq);

		write_string(ab);
	}

	// Use the ESC [6n escape sequence to query the horizontal cursor position and return it.
	// On error -1 is returned, on success the position of the cursor.
	int getCursorPosition() {
		char buf[32];
		int cols;
		int rows;
		unsigned int i = 0;

		// Report cursor location
		if (write_string("\x1b[6n") != 4) {
			return -1;
		}

		// Read the response: ESC [ rows ; cols R
		while (i < sizeof(buf) - 1) {
			if (read(in_fd_, buf + i, 1) != 1)
				break;
			if (buf[i] == 'R')
				break;
			i++;
		}
		buf[i] = '\0';

		// Parse it.
		if (buf[0] != ESC || buf[1] != '[')
			return -1;
		if (sscanf(buf + 2, "%d;%d", &rows, &cols) != 2)
			return -1;
		return cols;
	}

	// Try to get the number of columns in the current terminal, or assume 80 if it fails.
	int getColumns() {
		struct winsize ws;

		if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
			// ioctl() failed. Try to query the terminal itself.
			int start, cols;

			// Get the initial position so we can restore it later.
			start = getCursorPosition();
			if (start == -1)
				return 80;

			// Go to right margin and get position.
			if (write_string("\x1b[999C") != 6)
				return 80;
			cols = getCursorPosition();
			if (cols == -1)
				return 80;

			// Restore position.
			if (cols > start) {
				char seq[32];
				snprintf(seq, sizeof(seq), "\x1b[%dD", cols - start);
				if (write_string(seq) == -1) {
					// Can't recover...
				}
			}
			return cols;
		} else {
			return ws.ws_col;
		}
	}

	// This is an helper function for editLine() and is called when the user types the <tab> key in order to complete the string currently in the input.
	// The state of the editing is encapsulated into the pointed State structure as described in the structure definition.
	int completeLine(State *ls) {

		char ch = 0;

		// get a list of commands
		std::vector<std::string> completions;
		completions.reserve(commands_.size());
		for (auto &&elem : commands_) {
			const std::string &name = elem.first;

			if (starts_with(name, ls->buf)) {
				completions.push_back(name);
			}
		}

		if (completions.empty()) {
			beep();
		} else {
			bool stop = false;
			size_t i  = 0;

			while (!stop) {
				// Show completion or original buffer
				if (i < completions.size()) {
					State saved = *ls;

					ls->pos = completions[i].size();
					ls->buf = completions[i];

					refreshLine(ls);
					ls->pos = saved.pos;
					ls->buf = saved.buf;
				} else {
					refreshLine(ls);
				}

				int nread = read(in_fd_, &ch, 1);
				if (nread <= 0) {
					return -1;
				}

				switch (ch) {
				case TAB: // tab
					i = (i + 1) % (completions.size() + 1);
					if (i == completions.size())
						beep();
					break;
				case ESC: // escape
					// Re-show original buffer
					if (i < completions.size())
						refreshLine(ls);
					stop = true;
					break;
				default:
					// Update buffer and return
					if (i < completions.size()) {
						ls->buf = completions[i];
						ls->pos = ls->buf.size();
					}
					stop = true;
					break;
				}
			}
		}

		return ch; // Return last read character
	}

	bool addHistoryEntry(const std::string &line) {

		// Don't add duplicated lines.
		if (!history_.empty() && history_.back() == line) {
			return false;
		}

		history_.push_back(line);
		return true;
	}

	// Helper of refreshSingleLine() and refreshMultiLine() to show hints to the right of the prompt.
	void refreshShowHints(std::string &ab, State *state, int plen) {

		if (hints_handler_ && plen + state->buf.size() < state->cols) {
			int color = -1;
			bool bold = false;

			if (std::optional<std::string> hint = hints_handler_(state->buf, &color, &bold)) {
				if (bold && color == -1) {
					color = 37;
				}

				char seq[64];
				if (color != -1 || bold) {
					snprintf(seq, sizeof(seq), "\033[%d;%d;49m", bold, color);
				} else {
					seq[0] = '\0';
				}

				ab.append(seq);
				ab.append(*hint);
				if (color != -1 || bold)
					ab.append("\033[0m", 4);
			}
		}
	}

	ssize_t write_string(std::string_view str) const {
		return write(out_fd_, str.data(), str.size());
	}

	ssize_t write_char(char ch) const {
		return write(out_fd_, &ch, 1);
	}

public:
	std::string prompt = "(Cmd) ";
	bool mask_mode     = false; // Show "***" instead of input. For passwords.
	bool raw_mode      = false;

private:
	std::string identchars_ = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
	FILE *in_               = stdin;
	FILE *out_              = stdout;
	int complete_key_       = TAB;
	int in_fd_              = STDIN_FILENO;
	int out_fd_             = STDOUT_FILENO;

	std::queue<std::string> cmdqueue_;
	std::map<std::string, command_type> commands_;
	hints_type hints_handler_;
	struct termios orig_termios_; // In order to restore at exit.
	std::vector<std::string> history_;
	std::optional<std::string> history_file_;
};

#endif
