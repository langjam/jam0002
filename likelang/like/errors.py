from colorama import Back, Fore, Style, init

from lark.exceptions import UnexpectedInput

init()


def error_print(type: str, s: str):
    print(f"{Fore.RED}{type}:{Style.RESET_ALL} {s}")


def syntax_error(s: str):
    error_print("Syntax Error", s)


def runtime_error(s: str):
    error_print("Runtime Error", s)


def get_context(u: UnexpectedInput, text: str):
    pos = u.pos_in_stream

    start = pos

    while start >= 0:
        if text[start] == "\n":
            break
        start -= 1

    if start == -1:
        start += 1

    if text[start] == "\n":
        start += 1

    end = pos

    while end < len(text):
        if text[end] == "\n":
            break
        end += 1

    if end == len(text):
        end -= 1

    line = text[start:end]

    s = line.expandtabs()

    err = (
        f"{Fore.BLACK}{Back.WHITE}{s}{Style.RESET_ALL}\n"
        + " " * (len(text[start:pos].expandtabs()))
        + f"{Fore.BLUE}^{Style.RESET_ALL}"
    )

    return err

