from json import JSONEncoder, dumps

from TexSoup.data import TexNode, TexArgs, BraceGroup, BracketGroup
from TexSoup.utils import Token
from TexSoup import TexSoup
from sympy import srepr, And
from sympy.parsing.latex import parse_latex
from utils import MATH_MODE_ENV, remove_trailing_dollars, split_to_words, is_sublist, merge_around_multiple_separators
from utils import has_relation, parse_inequalities, split_and_filter_non_empty

ASSETS_BASE_DIR = f"assets"
TEX_BASE_DIR = f"{ASSETS_BASE_DIR}/tex"
JSON_BASE_DIR = f"{ASSETS_BASE_DIR}/json"


def read_latex(file_name: str) -> TexNode:
    """
    Read latex to TexSoup

    :param str file_name: File name
    :return: TexNode
    """
    with open(f"{TEX_BASE_DIR}/{file_name}", 'r') as latex_file:
        return TexSoup(latex_file.read())


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
                    formatted = remove_trailing_dollars(str(obj))
                    parsed = None;
                    if has_relation(formatted):
                        parsed = srepr(And(*parse_inequalities(formatted)))
                    else: 
                        parsed = srepr(parse_latex(formatted))
                    (type, sympy_exprs) = ("Math", parsed)
                return {"type": type, "value": sympy_exprs}
            
            elif splits[0] == "\\":
                if splits[1] == 'begin': 
                    if splits[3] in MATH_MODE_ENV and not is_sublist(splits[3:], ['\\', 'begin', '{']):               

                        merged_multiline_math = [element for element in merge_around_multiple_separators(obj.contents, r"\\") if element]
                        multiline_math = split_and_filter_non_empty(merged_multiline_math[0])
                        
                        parsed_multimaths = []
                        
                        for math_element in multiline_math:
                            if has_relation(math_element):
                                parsed = srepr(And(*parse_inequalities(math_element)))
                                parsed_multimaths.append(parsed)
                                
                            else: 
                                parsed = srepr(parse_latex(math_element))
                                parsed_multimaths.append(parsed)
                            
                        return {
                            "type": "Environment",
                            "name": splits[3],
                            "children": [
                                {
                                "type": "MultilineMath",
                                "value": parsed_multimaths
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
        return {"type": "Text", "value": (str(obj))}
    elif isinstance(obj, BracketGroup):
        return {"type": "Text", "value": (str(obj))}
    elif isinstance(obj, Token):
        if obj.strip().startswith("%"):
            return {"type": "Comment", "value": obj}
        return {"type": "Text", "value": obj}
    return str(obj)


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

def count_occurrences(main_string, substring):
    return main_string.count(substring)

if __name__ == "__main__":
    latex: TexSoup = read_latex("sample2.tex")
    # print('latex', srepr(parse_latex(r'x \in \mathbb{N}')))
    # print('latex', srepr(parse_latex(r'y \subseteq \mathbb{N}')))
    # print('latex', srepr(parse_latex(r'')))

    json_str = dumps(latex, cls=TexJsonEncoder, indent=2)
    with open(f"{JSON_BASE_DIR}/parsed-latex.json", 'w') as json_file:
        json_file.write(json_str)
