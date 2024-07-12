import string
from json import JSONEncoder, dumps
from string import whitespace
import TexSoup
from TexSoup.data import TexNode, TexArgs, BraceGroup, BracketGroup
from TexSoup.utils import Token
from TexSoup import TexSoup
# from latex2sympy2 import latex2sympy
from sympy import srepr
from sympy.parsing.latex import parse_latex

ASSETS_BASE_DIR = f"assets"
TEX_BASE_DIR = f"{ASSETS_BASE_DIR}/tex"
JSON_BASE_DIR = f"{ASSETS_BASE_DIR}/json"
MATH_MODE_ENV = ['equation', 'split', 'gather']

def indent(substr: str) -> str:
    return "\t" + substr.replace("\n", "\n\t")

def remove_first_and_last(s):
    """
    Remove the first and last characters of a string.

    :param str s: Input string
    :return: String with first and last characters removed
    :rtype: str
    """
    if len(s) <= 2:
        return ''
    return s[1:-1]
def read_latex(file_name: str) -> TexNode:
    """
    Read latex to TexSoup

    :param str file_name: File name
    :return: TexNode
    """
    with open(f"{TEX_BASE_DIR}/{file_name}", 'r') as latex_file:
        return TexSoup(latex_file.read())

def split_to_words(text: str) -> list:
    words = []
    buffer = []
    for c in text:
        if c in whitespace:
            if buffer:
                words.append("".join(buffer))
                buffer.clear()
        elif c in string.punctuation:
            if buffer:
                words.append("".join(buffer))
                buffer.clear()
            words.append(c)
        else:
            buffer.append(c)
    if buffer:
        words.append("".join(buffer))
    return words

def json_like_nonprim_encode(obj):
    if isinstance(obj, TexNode):        
        if obj.document:
            return { 
                "type": "Latex",
                "children": [json_like_encode(child) for child in obj.contents]
            }
        else:
            splits = split_to_words(str(obj))
                   
            if splits[0] == '$' and splits[len(splits) - 1] == '$':
                multiline_math = [element for element in str(obj).split(r"\\") if element]
        
                if (len(multiline_math) > 1):
                    (type, sympy_exprs) = ("MultilineMath", [srepr(parse_latex(math_element)) for math_element in multiline_math])
                else: 
                    (type, sympy_exprs) = ("Math", srepr(parse_latex(remove_first_and_last(str(obj)))))
                return {"type": type, "value": sympy_exprs}
            
            elif splits[0] == "\\":
                if splits[1] == 'begin': 
                    if splits[3] in MATH_MODE_ENV and not is_sublist(splits[3:], ['\\', 'begin', '{']):               
                        print("conte", obj.contents)
                        # multiline_math = [element for element in str(obj.).split(r"\\") if element]
                        multiline_math = [element for element in merge_around_multiple_separators(obj.contents, r"\\") if element]
                        
                        return {
                            "type": "Environment",
                            "name": splits[3],
                            "children": [
                                {
                                "type": "MultilineMath",
                                "value": [srepr(parse_latex(math_element)) for math_element in multiline_math]
                                }
                            ]
                        }
                        
                    return {"type": "Environment", "name": splits[3], "children": [json_like_encode(child) for child in obj.contents]}
                else:
                    return { 
                                "type": "Macro",
                                "name": splits[1],
                                "args": [json_like_encode(child) for child in obj.args.all] 
                            }
    elif isinstance(obj, BraceGroup):
        return {"type": "Text", "value": remove_first_and_last(str(obj))}
    elif isinstance(obj, BracketGroup):
        return {"type": "Text", "value": remove_first_and_last(str(obj))}
    elif isinstance(obj, Token):
        print("Obj: ", obj.strip().startswith("%"))
        if obj.strip().startswith("%"):
            return {"type": "Comment", "value": obj}
        return {"type": "Text", "value": obj}
    return str(obj)

# def unwrap_multiline_math(obj):
#     splits = split_to_words(str(obj))
#     print("Splitting", splits);
#     print("Unwrapping", obj)
#     if splits[1] == 'begin': 
#         if splits[3] in MATH_MODE_ENV:
   
def is_sublist(main_list, sub_list):
    len_main = len(main_list)
    len_sub = len(sub_list)
    
    for i in range(len_main - len_sub + 1):
        if main_list[i:i + len_sub] == sub_list:
            return True
    return False

def merge_around_multiple_separators(input_list, separator):
    merged_strings = []
    current_string = []

    for item in input_list:
        if item == separator:
            if current_string:
                merged_strings.append(''.join(map(str, current_string)))
                current_string = []
        else:
            current_string.append(item)

    if current_string:
        merged_strings.append(''.join(map(str, current_string)))

    return merged_strings
    
def json_like_encode(obj):
    if isinstance(obj, (str, int, float)):
        return json_like_nonprim_encode(obj)
    elif isinstance(obj, list):
        return [json_like_encode(e) for e in obj]
    elif isinstance(obj, dict):
        return {key: json_like_encode(val) for key, val in obj.items()}
    elif isinstance(obj, TexNode):
        return json_like_nonprim_encode(obj)
    elif isinstance(obj, TexArgs):
        return json_like_nonprim_encode(obj)
    else:
        return json_like_nonprim_encode(obj)

class TexJsonEncoder(JSONEncoder):
    def default(self, obj):
        return json_like_encode(obj)

if __name__ == "__main__":
    latex: TexSoup = read_latex("sample1.tex")
    json_str = dumps(latex, cls=TexJsonEncoder, indent=2)
    with open(f"{JSON_BASE_DIR}/parsed-latex.json", 'w') as json_file:
        json_file.write(json_str)
    print(json_str)
