
#ifndef CMD_H_
#define CMD_H_

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <functional>
#include <map>
#include <optional>
#include <queue>
#include <string>
#include <string_view>
#include <vector>

#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

// trim from start (in place)
static inline void ltrim(std::string &s) {
	s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) { return !std::isspace(ch); }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
	s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) { return !std::isspace(ch); }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
	ltrim(s);
	rtrim(s);
}

static inline bool starts_with(std::string_view s, std::string_view prefix) {
	return std::mismatch(prefix.begin(), prefix.end(), s.begin()).first == prefix.end();
}

class Cmd {
private:
	static constexpr int MaxLineLength = 4096;

	using completer_type = std::function<std::vector<std::string>(std::string_view)>;
	using hints_type     = std::function<std::optional<std::string>(std::string_view, int *, bool *)>;
	using command_type   = std::function<bool(std::string_view)>;

	// The State structure represents the state during line editing. We pass this state to functions implementing specific editing functionalities.
	struct State {
		char *buf;            // Edited line buffer.
		size_t buflen;        // Edited line buffer size.
		size_t pos;           // Current cursor position.
		size_t oldpos;        // Previous refresh cursor position.
		size_t len;           // Current edited line length.
		size_t cols;          // Number of columns in terminal.
		size_t maxrows;       // Maximum num of rows used so far (multiline mode)
		size_t history_index; // The history index we are currently editing.
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
		CTRL_K    = 11, // Ctrl+k
		CTRL_L    = 12, // Ctrl+l
		ENTER     = 13, // Enter
		CTRL_N    = 14, // Ctrl-n
		CTRL_P    = 16, // Ctrl-p
		CTRL_T    = 20, // Ctrl-t
		CTRL_U    = 21, // Ctrl+u
		CTRL_W    = 23, // Ctrl+w
		ESC       = 27, // Escape
		BACKSPACE = 127 // Backspace
	};

public:
	Cmd(int complete_key = TAB, FILE *in = stdin, FILE *out = stdout)
		: in_(in), out_(out), complete_key_(complete_key) {

		in_fd_  = fileno(in_);
		out_fd_ = fileno(out_);
	}

	~Cmd() {
		// At exit we'll try to fix the terminal to the initial conditions.
		disableRawMode(in_fd_);
	}

public:
	// Register a hits function to be called to show hits to the user at the right of the prompt.
	void setHintsCallback(hints_type fn) {
		hints_handler_ = fn;
	}

	// This function is called when linenoise() is called with the standard input file descriptor not attached to a TTY. So for example when the program using linenoise is called in pipe or with a file redirected to its standard input. In this case, we want to be able to return the line regardless of its length (by default we are limited to 4k).
	std::string linenoiseNoTTY() {
		std::string line;

		while (true) {
			int c = fgetc(in_);
			if (c == EOF || c == '\n') {
				return line;
			} else {
				line.push_back(c);
			}
		}
	}

	void cmdloop(std::string_view intro = {}) {
		char buf[MaxLineLength];

		if (!intro.empty()) {
			write_string(intro);
			fputc('\n', out_);
		}

		bool stop = false;

		while (!stop) {

			if (!isatty(in_fd_)) {
				// Not a tty: read from file / pipe. In this mode we don't want any limit to the line size, so we call a function to handle that.
				std::string line = linenoiseNoTTY();
				stop             = onecmd(line);

			} else if (isUnsupportedTerm()) {

				write_string(prompt);
				fflush(out_);

				if (fgets(buf, MaxLineLength, in_) == nullptr) {
					break;
				}

				size_t len = strlen(buf);
				while (len && (buf[len - 1] == '\n' || buf[len - 1] == '\r')) {
					len--;
					buf[len] = '\0';
				}

				stop = onecmd(buf);

			} else {
				const int count = linenoiseRaw(buf, MaxLineLength);
				if (count < 0) {
					break;
				}

				stop = onecmd(buf);
			}
		}
	}

	bool onecmd(const std::string &str) {
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
			return onecmd(history_.back());
		}

		return false;
	}

	bool defaultHandler(const std::string &line) {
		write_string("*** Unknown command: ");
		write_string(line);
		write_char('\n');
		return false;
	}

private:
	struct parse_result {
		std::string cmd;
		std::string args;
		std::string line;
	};

	parse_result parseline(std::string line) const {

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
		return parse_result{cmd, args, line};
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

	// This function calls the line editing function linenoiseEdit() using the STDIN file descriptor set in raw mode.
	int linenoiseRaw(char *buf, size_t buflen) {

		if (buflen == 0) {
			return -EINVAL;
		}

		int r = enableRawMode(in_fd_);
		if (r < 0) {
			return r;
		}

		int count = linenoiseEdit(buf, buflen);
		disableRawMode(in_fd_);
		write_char('\n');
		return count;
	}

	// Raw mode: 1960 magic shit.
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
		// local modes - choing off, canonical off, no extended functions, no signal chars (^Z,^C)
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

	// This function is the core of the line editing capability of linenoise. It expects 'fd' to be already in "raw mode" so that every key pressed will be returned ASAP to read().
	// The resulting string is put into 'buf' when the user type enter, or when ctrl+d is typed.
	// The function returns the length of the current buffer.
	int linenoiseEdit(char *buf, size_t buflen) {
		State l;

		// Populate the linenoise state that we pass to functions implementing specific editing functionalities.
		l.buf    = buf;
		l.buflen = buflen;
		l.oldpos = l.pos = 0;
		l.len            = 0;
		l.cols           = getColumns();
		l.maxrows        = 0;
		l.history_index  = 0;

		// Buffer starts empty.
		l.buf[0] = '\0';
		l.buflen--; // Make sure there is always space for the nulterm

		// The latest history entry is always our current buffer, that initially is just an empty string.
		addHistoryEntry("");

		if (write_string(prompt) == -1) {
			return -1;
		}

		while (true) {
			char c;
			char seq[3];

			int nread = read(in_fd_, &c, 1);
			if (nread <= 0) {
				return l.len;
			}

			// Only autocomplete when the callback is set. It returns < 0 when there was an error reading from fd.
			// Otherwise it will return the character that should be handled next.
			if (c == complete_key_) {
				c = completeLine(&l);
				// Return on errors
				if (c < 0)
					return l.len;
				// Read next character when 0
				if (c == 0)
					continue;
			}

			switch (c) {
			case ENTER: // enter
				history_.pop_back();
				if (multi_line_mode) {
					linenoiseEditMoveEnd(&l);
				}

				if (hints_handler_) {
					// Force a refresh without hints to leave the previous line as the user typed it after a newline.
					hints_type hc  = hints_handler_;
					hints_handler_ = nullptr;
					refreshLine(&l);
					hints_handler_ = hc;
				}
				return static_cast<int>(l.len);
			case CTRL_C: // ctrl-c
				return -EAGAIN;
			case BACKSPACE: // backspace
			case 8:         // ctrl-h
				linenoiseEditBackspace(&l);
				break;
			case CTRL_D: // ctrl-d, remove char at right of cursor, or if the line is empty, act as end-of-file.
				if (l.len > 0) {
					linenoiseEditDelete(&l);
				} else {
					history_.pop_back();
					return -1;
				}
				break;
			case CTRL_T: // ctrl-t, swaps current character with previous.
				if (l.pos > 0 && l.pos < l.len) {
					int aux        = buf[l.pos - 1];
					buf[l.pos - 1] = buf[l.pos];
					buf[l.pos]     = aux;
					if (l.pos != l.len - 1)
						l.pos++;
					refreshLine(&l);
				}
				break;
			case CTRL_B: // ctrl-b
				linenoiseEditMoveLeft(&l);
				break;
			case CTRL_F: // ctrl-f
				linenoiseEditMoveRight(&l);
				break;
			case CTRL_P: // ctrl-p
				linenoiseEditHistoryNext(&l, Direction::Prev);
				break;
			case CTRL_N: // ctrl-n
				linenoiseEditHistoryNext(&l, Direction::Next);
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
								linenoiseEditDelete(&l);
								break;
							}
						}
					} else {
						switch (seq[1]) {
						case 'A': // Up
							linenoiseEditHistoryNext(&l, Direction::Prev);
							break;
						case 'B': // Down
							linenoiseEditHistoryNext(&l, Direction::Next);
							break;
						case 'C': // Right
							linenoiseEditMoveRight(&l);
							break;
						case 'D': // Left
							linenoiseEditMoveLeft(&l);
							break;
						case 'H': // Home
							linenoiseEditMoveHome(&l);
							break;
						case 'F': // End
							linenoiseEditMoveEnd(&l);
							break;
						}
					}
				}

				// ESC O sequences.
				else if (seq[0] == 'O') {
					switch (seq[1]) {
					case 'H': // Home
						linenoiseEditMoveHome(&l);
						break;
					case 'F': // End
						linenoiseEditMoveEnd(&l);
						break;
					}
				}
				break;
			default:
				if (linenoiseEditInsert(&l, c)) {
					return -1;
				}
				break;
			case CTRL_U: // Ctrl+u, delete the whole line.
				buf[0] = '\0';
				l.pos = l.len = 0;
				refreshLine(&l);
				break;
			case CTRL_K: // Ctrl+k, delete from current to end of line.
				buf[l.pos] = '\0';
				l.len      = l.pos;
				refreshLine(&l);
				break;
			case CTRL_A: // Ctrl+a, go to the start of the line
				linenoiseEditMoveHome(&l);
				break;
			case CTRL_E: // ctrl+e, go to the end of the line
				linenoiseEditMoveEnd(&l);
				break;
			case CTRL_L: // ctrl+l, clear screen
				clearScreen();
				refreshLine(&l);
				break;
			case CTRL_W: // ctrl+w, delete previous word
				linenoiseEditDeletePrevWord(&l);
				break;
			}
		}
		return l.len;
	}

	// Insert the character 'c' at cursor current position. On error writing to the terminal -1 is returned, otherwise 0.
	int linenoiseEditInsert(State *l, char c) {
		if (l->len < l->buflen) {
			if (l->len == l->pos) {
				l->buf[l->pos] = c;
				l->pos++;
				l->len++;
				l->buf[l->len] = '\0';
				if ((!multi_line_mode && prompt.size() + l->len < l->cols && !hints_handler_)) {
					// Avoid a full update of the line in the trivial case.
					const char d = (mask_mode) ? '*' : c;
					if (write_char(d) == -1) {
						return -1;
					}
				} else {
					refreshLine(l);
				}
			} else {
				memmove(l->buf + l->pos + 1, l->buf + l->pos, l->len - l->pos);
				l->buf[l->pos] = c;
				l->len++;
				l->pos++;
				l->buf[l->len] = '\0';
				refreshLine(l);
			}
		}
		return 0;
	}

	// Move cursor on the left.
	void linenoiseEditMoveLeft(State *l) {
		if (l->pos > 0) {
			l->pos--;
			refreshLine(l);
		}
	}

	// Move cursor on the right.
	void linenoiseEditMoveRight(State *l) {
		if (l->pos != l->len) {
			l->pos++;
			refreshLine(l);
		}
	}

	// Move cursor to the start of the line.
	void linenoiseEditMoveHome(State *l) {
		if (l->pos != 0) {
			l->pos = 0;
			refreshLine(l);
		}
	}

	// Move cursor to the end of the line.
	void linenoiseEditMoveEnd(State *l) {
		if (l->pos != l->len) {
			l->pos = l->len;
			refreshLine(l);
		}
	}

	// Substitute the currently edited line with the next or previous history entry as specified by 'dir'.
	void linenoiseEditHistoryNext(State *l, Direction dir) {

		if (!history_.empty()) {
			// Update the current history entry before to overwrite it with the next one.
			history_[history_.size() - 1 - l->history_index] = l->buf;

			switch (dir) {
			case Direction::Prev:
				++l->history_index;
				if (l->history_index >= history_.size()) {
					l->history_index = history_.size() - 1;
					return;
				}
				break;
			case Direction::Next:
				if (l->history_index == 0) {
					return;
				}
				--l->history_index;
			}

			// Show the new entry
			strncpy(l->buf, history_[history_.size() - 1 - l->history_index].c_str(), l->buflen);
			l->buf[l->buflen - 1] = '\0';
			l->len = l->pos = strlen(l->buf);
			refreshLine(l);
		}
	}

	// Delete the character at the right of the cursor without altering the cursor position.
	// Basically this is what happens with the "Delete" keyboard key.
	void linenoiseEditDelete(State *l) {
		if (l->len > 0 && l->pos < l->len) {
			memmove(l->buf + l->pos, l->buf + l->pos + 1, l->len - l->pos - 1);
			l->len--;
			l->buf[l->len] = '\0';
			refreshLine(l);
		}
	}

	// Backspace implementation.
	void linenoiseEditBackspace(State *l) {
		if (l->pos > 0 && l->len > 0) {
			memmove(l->buf + l->pos - 1, l->buf + l->pos, l->len - l->pos);
			l->pos--;
			l->len--;
			l->buf[l->len] = '\0';
			refreshLine(l);
		}
	}

	// Delete the previosu word, maintaining the cursor at the start of the current word.
	void linenoiseEditDeletePrevWord(State *l) {
		size_t old_pos = l->pos;
		size_t diff;

		while (l->pos > 0 && l->buf[l->pos - 1] == ' ')
			l->pos--;

		while (l->pos > 0 && l->buf[l->pos - 1] != ' ')
			l->pos--;

		diff = old_pos - l->pos;
		memmove(l->buf + l->pos, l->buf + old_pos, l->len - old_pos + 1);
		l->len -= diff;
		refreshLine(l);
	}

	// Calls the two low level functions refreshSingleLine() or refreshMultiLine() according to the selected mode.
	void refreshLine(State *l) {
		if (multi_line_mode)
			refreshMultiLine(l);
		else
			refreshSingleLine(l);
	}

	// Multi line low level line refresh. Rewrite the currently edited line accordingly to the buffer content, cursor position, and number of columns of the terminal.
	void refreshMultiLine(State *l) {
		char seq[64];

		int rows = (prompt.size() + l->len + l->cols - 1) / l->cols; // rows used by current buf.
		int rpos = (prompt.size() + l->oldpos + l->cols) / l->cols;  // cursor relative row.

		int rpos2; // rpos after refresh.
		int old_rows = l->maxrows;
		int j;
		std::string ab;

		// Update maxrows if needed.
		if (rows > static_cast<int>(l->maxrows))
			l->maxrows = rows;

		// First step: clear all the lines used before. To do so start by going to the last row.
		if (old_rows - rpos > 0) {
			snprintf(seq, sizeof(seq), "\x1b[%dB", old_rows - rpos);
			ab.append(seq);
		}

		// Now for every row clear it, go up.
		for (j = 0; j < old_rows - 1; j++) {
			snprintf(seq, sizeof(seq), "\r\x1b[0K\x1b[1A");
			ab.append(seq);
		}

		// Clean the top line.
		snprintf(seq, sizeof(seq), "\r\x1b[0K");
		ab.append(seq);

		// Write the prompt and the current buffer content
		ab.append(prompt);
		if (mask_mode) {
			ab.append(l->len, '*');
		} else {
			ab.append(l->buf, l->len);
		}

		// Show hits if any.
		refreshShowHints(ab, l, prompt.size());

		// If we are at the very end of the screen with our prompt, we need to emit a newline and move the prompt to the first column.
		if (l->pos && l->pos == l->len && (l->pos + prompt.size()) % l->cols == 0) {
			ab.append("\n");
			snprintf(seq, sizeof(seq), "\r");
			ab.append(seq);
			rows++;
			if (rows > static_cast<int>(l->maxrows)) {
				l->maxrows = rows;
			}
		}

		// Move cursor to right position.
		rpos2 = (prompt.size() + l->pos + l->cols) / l->cols; // current cursor relative row.

		// Go up till we reach the expected positon.
		if (rows - rpos2 > 0) {
			snprintf(seq, sizeof(seq), "\x1b[%dA", rows - rpos2);
			ab.append(seq);
		}

		// Set column.
		const size_t col = (prompt.size() + l->pos) % l->cols;
		if (col)
			snprintf(seq, sizeof(seq), "\r\x1b[%dC", static_cast<int>(col));
		else
			snprintf(seq, sizeof(seq), "\r");
		ab.append(seq);

		l->oldpos = l->pos;

		write_string(ab);
	}

	// Single line low level line refresh. Rewrite the currently edited line accordingly to the buffer content, cursor position, and number of columns of the terminal.
	void refreshSingleLine(State *l) {
		char seq[64];
		const size_t plen = prompt.size();
		char *buf         = l->buf;
		size_t len        = l->len;
		size_t pos        = l->pos;
		std::string ab;

		while ((plen + pos) >= l->cols) {
			buf++;
			len--;
			pos--;
		}

		while (plen + len > l->cols) {
			len--;
		}

		// Cursor to left edge
		snprintf(seq, sizeof(seq), "\r");
		ab.append(seq);
		// Write the prompt and the current buffer content
		ab.append(prompt);

		if (mask_mode) {
			ab.append(len, '*');
		} else {
			ab.append(buf, len);
		}
		// Show hits if any.
		refreshShowHints(ab, l, plen);
		// Erase to right
		snprintf(seq, sizeof(seq), "\x1b[0K");
		ab.append(seq);
		// Move cursor to original position.
		snprintf(seq, sizeof(seq), "\r\x1b[%dC", static_cast<int>(pos + plen));
		ab.append(seq);

		write_string(ab);
	}

	// Clear the screen. Used to handle ctrl+l
	void clearScreen() {
		write_string("\x1b[H\x1b[2J");
	}

	// Beep, used for completion when there is nothing to complete or when all the choices were already shown.
	static void beep() {
		fprintf(stderr, "\x7");
		fflush(stderr);
	}

	void disableRawMode(int fd) {
		// Don't even check the return value as it's too late.
		if (raw_mode && tcsetattr(fd, TCSAFLUSH, &orig_termios_) != -1) {
			raw_mode = false;
		}
	}

	// Use the ESC [6n escape sequence to query the horizontal cursor position and return it. On error -1 is returned, on success the position of the cursor.
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

	// This is an helper function for linenoiseEdit() and is called when the user types the <tab> key in order to complete the string currently in the input.
	// The state of the editing is encapsulated into the pointed State structure as described in the structure definition.
	int completeLine(State *ls) {

		char c = 0;

		// get a list of commands
		std::vector<std::string> lc;
		lc.reserve(commands_.size());
		for (auto &&elem : commands_) {
			const std::string &name = elem.first;

			if (starts_with(name, ls->buf)) {
				lc.push_back(name);
			}
		}

		if (lc.empty()) {
			beep();
		} else {
			bool stop = false;
			size_t i  = 0;

			while (!stop) {
				// Show completion or original buffer
				if (i < lc.size()) {
					State saved = *ls;

					ls->len = ls->pos = lc[i].size();

					strncpy(ls->buf, lc[i].c_str(), ls->buflen);
					ls->buf[ls->buflen - 1] = '\0';

					refreshLine(ls);
					ls->len = saved.len;
					ls->pos = saved.pos;
					ls->buf = saved.buf;
				} else {
					refreshLine(ls);
				}

				int nread = read(in_fd_, &c, 1);
				if (nread <= 0) {
					return -1;
				}

				switch (c) {
				case TAB: // tab
					i = (i + 1) % (lc.size() + 1);
					if (i == lc.size())
						beep();
					break;
				case ESC: // escape
					// Re-show original buffer
					if (i < lc.size())
						refreshLine(ls);
					stop = true;
					break;
				default:
					// Update buffer and return
					if (i < lc.size()) {
						int nwritten = snprintf(ls->buf, ls->buflen, "%s", lc[i].c_str());
						ls->len = ls->pos = nwritten;
					}
					stop = true;
					break;
				}
			}
		}

		return c; // Return last read character
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
	void refreshShowHints(std::string &ab, State *l, int plen) {

		if (hints_handler_ && plen + l->len < l->cols) {
			int color = -1;
			bool bold = false;

			if (std::optional<std::string> hint = hints_handler_(l->buf, &color, &bold)) {
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

public:
	void registerCommand(const std::string &cmd, command_type handler) {
		commands_.emplace(cmd, handler);
	}

private:
	ssize_t write_string(std::string_view str) const {
		return write(out_fd_, str.data(), str.size());
	}

	ssize_t write_char(char ch) const {
		return write(out_fd_, &ch, 1);
	}

public:
	std::string prompt   = "(Cmd) ";
	bool mask_mode       = false; // Show "***" instead of input. For passwords.
	bool raw_mode        = false; // For atexit() function to check if restore is needed
	bool multi_line_mode = false; // Multi line mode. Default is single line.

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
};

#endif
