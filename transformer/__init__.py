import string
from json import JSONEncoder, dumps
from string import whitespace

from TexSoup.data import TexNode, TexArgs, BraceGroup, BracketGroup
from TexSoup.utils import Token
from TexSoup import TexSoup
from sympy import srepr, And
from sympy.parsing.latex import parse_latex

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
            # print('splits', splits[3:])

            if splits[0] == '$' and splits[len(splits) - 1] == '$':
  
                multiline_math = [element for element in str(obj).split(r"\\") if element]
        
                if (len(multiline_math) > 1):
                    (type, sympy_exprs) = ("MultilineMath", [srepr(parse_latex(math_element)) for math_element in multiline_math])
                else: 
                    formatted = remove_trailing_dollars(str(obj))
                    print("Formatted", formatted)
                    print("Obj", obj)
                    print("splits", splits)
                    (type, sympy_exprs) = ("Math", srepr(parse_latex(remove_trailing_dollars(formatted))))
                return {"type": type, "value": sympy_exprs}
            
            elif splits[0] == "\\":
                if splits[1] == 'begin': 
                    if splits[3] in MATH_MODE_ENV and not is_sublist(splits[3:], ['\\', 'begin', '{']):               
    
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
        return {"type": "Text", "value": (str(obj))}
    elif isinstance(obj, BracketGroup):
        return {"type": "Text", "value": (str(obj))}
    elif isinstance(obj, Token):
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
    latex: TexSoup = read_latex("sample3.tex")
    # print('latex', srepr(parse_latex(r'${x \vert x \in L}$')))
    
    # print('latex', srepr(parse_latex(r'y \subseteq \mathbb{N}')))
    
        # Input compound inequality
    compound_inequality = 'a \leq d \leq 9 \leq u'

    # Split into individual inequalities
    rela
    if count_occurrences(compound_inequality, r'\leq') > 1:
        parts = compound_inequality.split(r'\leq')

        # Parse each part separately
        parsed_inequalities = [parse_latex(part.strip()) for part in parts]

        # Combine the inequalities using logical operators
        inequalities = []
        for i in range(len(parsed_inequalities) - 1):
            inequalities.append(parsed_inequalities[i] < parsed_inequalities[i + 1])

        # Combine all inequalities using And
        combined_inequality = And(*inequalities)

        print(srepr(combined_inequality))
    else if count_occurrences(compound_inequality, ): 
        print (srepr(parse_latex(compound_inequality)))

    # print('latex', srepr(parse_latex(r'1 \leq e \leq d')))
    # json_str = dumps(latex, cls=TexJsonEncoder, indent=2)
    # with open(f"{JSON_BASE_DIR}/parsed-latex.json", 'w') as json_file:
    #     json_file.write(json_str)
