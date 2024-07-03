import itertools
import string
from json import JSONEncoder, dumps
from string import whitespace

import TexSoup as soup
import TexSoup.utils as soup_utils
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


def json_like_nonprim_encode(obj):
    match obj:
        case soup.data.TexNode(name="[tex]"):
            return {"Type": "Latex", "Children": obj.children}
        case soup.data.TexNode(name="$"):
            return {"Type": "Math", "Value": str(obj)}
        case _:
            print(f"UNKNOWN\nType: {type(obj)}\nobj: {obj}\n")
        # case soup.utils.Token():
        #     return {
        #         "Type": "Group",
        #         "Children": [{
        #             "Type": "Word",
        #             "Value": json_like_encode(word)
        #         } for word in split_to_words(str(obj))]
        #     }


def json_like_encode(obj):
    match obj:
        case None | str() | int() | float():
            return obj
        case list():
            return [json_like_encode(e) for e in obj]
        case dict():
            return {key: json_like_encode(val) for key, val in obj.items()}
        case _:
            return json_like_encode(json_like_nonprim_encode(obj))


class TexJsonEncoder(JSONEncoder):
    def default(self, obj):
        return json_like_encode(obj)


if __name__ == "__main__":
    latex = read_latex("sample.tex")
    json_str = TexJsonEncoder().default(latex)
    print(json_str)
