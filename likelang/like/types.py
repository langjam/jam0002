from dataclasses import dataclass
from typing import Any, List


@dataclass
class Variable:
    name: str
    value: Any

    def __repr__(self) -> str:
        return "Variable<{}: {}>".format(self.name, self.value)

    def __str__(self) -> str:
        return str(self.value)


@dataclass
class Function:
    name: str
    args: List
    value: Any

    def __repr__(self) -> str:
        return "Function<{}: {}>".format(self.name, self.args)


@dataclass
class Collect:
    value: Any
    type: str

    def __repr__(self) -> str:

        return "Collect<{} - {}>".format(
            self.type, ", ".join(["'" + f[0] + "'" for f in self.value])
        )
