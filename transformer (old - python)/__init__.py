import itertools
import string
from json import JSONEncoder
from string import whitespace

import TexSoup.utils
from TexSoup import TexSoup, TexNode


def indent(substr: str) -> str:
    return "\t" + substr.replace("\n", "\n\t")


def read_latex(file_name: str) -> TexNode:
    with open("sample.tex", 'r') as latex_file:
        return TexSoup(latex_file.read())


def split_to_words(text: str) -> list[str]:
    words = []
    buffer = []
    for c in text:
        if c in whitespace:
            words.append("".join(buffer))
            buffer.clear()
        elif c in string.punctuation:
            words.append("".join(buffer))
            words.append(c)
            buffer.clear()
        else:
            buffer.append(c)
    words.append("".join(buffer))
    return words


class TexJsonEncoder(JSONEncoder):
    def default(self, obj):
        match obj:
            case TexSoup.utils.Token():
                return {"Type": "Group",
                        "Children": [{"Type": "Word", "Value": word} for word in split_to_words(str(obj))]}
            case TexSoup.data.TexNode(name="$"):
                return {"Type": "Math", "Value": str(obj)}



if __name__ == "__main__":
    latex = read_latex("sample.tex")
