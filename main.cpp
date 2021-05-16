
#include <cmd-cpp/Cmd.h>
#include <iostream>

int main(int argc, char *argv[]) {

	(void)argc;
	(void)argv;

	Cmd cmd("history.txt");
	cmd.multiline_mode = true;

	cmd.setHintsCallback([](std::string_view buf) -> std::optional<Cmd::Hint> {
		if (buf == "git remote add") {
			Cmd::Hint r;
			r.hint_string = " <name> <url>";
			r.color = 90;
			r.bold = false;
			return r;
		}
		return {};
	});

	cmd.registerCommand("test", [](Cmd *cmd, std::string_view args) {
		(void)cmd;
		(void)args;
		printf("Hello World!\n");
		return false;
	});

	cmd.registerCommand("clear", [](Cmd *cmd, std::string_view args) {
		(void)args;
		cmd->clearScreen();
		return false;
	});

	cmd.registerCommand("exit", [](Cmd *cmd, std::string_view args) {
		(void)cmd;
		(void)args;
		return true;
	});

	cmd.prompt = "(Password) ";

	if (auto line = cmd.nextLine(true)) {
		std::cout << *line << std::endl;
	}

	cmd.prompt = "(Cmd) ";

	cmd.cmdLoop();
}
