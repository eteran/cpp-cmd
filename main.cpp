
#include <cmd-cpp/Cmd.h>

int main(int argc, char *argv[]) {

	(void)argc;
	(void)argv;

	Cmd cmd("history.txt");

	cmd.setHintsCallback([](std::string_view buf, int *color, bool *bold) -> std::optional<std::string> {
		if (buf == "git remote add") {
			*color = 90;
			*bold  = false;
			return " <name> <url>";
		}
		return {};
	});

	cmd.registerCommand("test", [](std::string_view args) {
		(void)args;
		printf("Hello World!\n");
		return false;
	});

	cmd.registerCommand("exit", [](std::string_view args) {
		(void)args;
		return true;
	});

	cmd.cmdLoop();
}
