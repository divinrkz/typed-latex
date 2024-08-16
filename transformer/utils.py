import string
from string import whitespace 
import re
from sympy.parsing.latex import parse_latex
from sympy import Interval, Contains, srepr, Symbol, And, sympify, Interval
from notations import SET_NOTATIONS, INEQUALITY_NOTATIONS, EQUALITY_NOTATIONS, MATH_SET_CONTAINER

MATH_MODE_ENV = ['equation', 'split', 'gather', 'multiline']


def indent(substr: str) -> str:
    return "\t" + substr.replace("\n", "\n\t")

def remove_trailing_dollars(input_data):
    if isinstance(input_data, str):
        return input_data.strip('$')
    elif isinstance(input_data, list):
        while input_data and input_data[0] == '$':
            input_data.pop(0)
        
        while input_data and input_data[-1] == '$':
            input_data.pop()
        return input_data
    else:
        return input_data

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
    
    
    
    
def extract_notations(notation_list):
    return [item['notation'] for item in notation_list]

def get_notation(name: str, notation_list):
    for item in notation_list:
        if item['name'] == name:
            return item['notation']
    return None

def extract_string_between_braces(latex_string):
    pattern = r'\{([^}]*)\}'
    matches = re.findall(pattern, latex_string)
    return matches[0]
    
def split_by_separators(string, notation_list):
    pattern = '|'.join([re.escape(rel) for rel in notation_list])
    return re.split(pattern, string)
    
def has_inequality_relation(string):
    escaped_relations = [re.escape(rel) for rel in extract_notations(INEQUALITY_NOTATIONS)]
    
    pattern = '|'.join(escaped_relations)
    return re.search(pattern, string) is not None

def has_equality_relation(string):
    escaped_relations = [re.escape(rel) for rel in extract_notations(EQUALITY_NOTATIONS)]
    
    pattern = '|'.join(escaped_relations)
    return re.search(pattern, string) is not None
    
def has_set_relation(string):
    escaped_relations = [re.escape(rel) for rel in extract_notations(SET_NOTATIONS)]
    pattern = '|'.join(escaped_relations)
    return re.search(pattern, string) is not None

def has_interval(interval_string):
    pattern = r'^[\[\(]\s*[^,\s]+\s*,\s*[^,\s]+\s*[\]\)]$'
    return re.match(pattern, interval_string) is not None

    
def parse_inequalities(compound_inequality):     
    parts = split_by_separators(compound_inequality, extract_notations(INEQUALITY_NOTATIONS))
    parsed_inequalities = []
    for part in parts:
        if r'\mathbb' in part:
            parsed_inequalities.append(f'Mathbb({srepr(Symbol(extract_string_between_braces(part.strip())))})')
        elif has_interval(part.strip()):
            parsed_inequalities.append(parse_interval(part.strip()))
        else: 
            parsed_inequalities.append((parse_latex(part.strip())))
    inequalities = []
    
    index = 0
    for separator in re.findall('|'.join([re.escape(rel) for rel in extract_notations(INEQUALITY_NOTATIONS)]), compound_inequality):
        if separator == '<':
            inequalities.append(parsed_inequalities[index] < parsed_inequalities[index + 1])
            
        elif separator == '>':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\leq':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\geq':
            inequalities.append(parsed_inequalities[index] >= parsed_inequalities[index + 1])
        elif separator == '\\leqq':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\leqslant':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\lesssim':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\lessapprox':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\prec':
            inequalities.append(parsed_inequalities[index] < parsed_inequalities[index + 1])
        elif separator == '\\preceq':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\lessgtr':
            inequalities.append(parsed_inequalities[index] != parsed_inequalities[index + 1])
        elif separator == '\\lesseqgtr':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\geqq':
            inequalities.append(parsed_inequalities[index] >= parsed_inequalities[index + 1])
        elif separator == '\\geqslant':
            inequalities.append(parsed_inequalities[index] >= parsed_inequalities[index + 1])
        elif separator == '\\gtrsim':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\gtrapprox':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\succ':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\succeq':
            inequalities.append(parsed_inequalities[index] >= parsed_inequalities[index + 1])
        elif separator == '\\succsim':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\nless':
            inequalities.append(parsed_inequalities[index] >= parsed_inequalities[index + 1])
        elif separator == '\\ngtr':
            inequalities.append(parsed_inequalities[index] <= parsed_inequalities[index + 1])
        elif separator == '\\nleq':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\ngeq':
            inequalities.append(parsed_inequalities[index] < parsed_inequalities[index + 1])
        elif separator == '\\lneq':
            inequalities.append(parsed_inequalities[index] < parsed_inequalities[index + 1])
        elif separator == '\\gneq':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\nleqslant':
            inequalities.append(parsed_inequalities[index] > parsed_inequalities[index + 1])
        elif separator == '\\ngeqslant':
            inequalities.append(parsed_inequalities[index] < parsed_inequalities[index + 1])
        elif separator == '\\ll':
            inequalities.append(parsed_inequalities[index] << parsed_inequalities[index + 1])
        elif separator == '\\lll':
            inequalities.append(parsed_inequalities[index] << parsed_inequalities[index + 1])
        elif separator == '\\gg':
            inequalities.append(parsed_inequalities[index] >> parsed_inequalities[index + 1])
        elif separator == '\\ggg':
            inequalities.append(parsed_inequalities[index] >> parsed_inequalities[index + 1])
        index += 1
    return inequalities
  
  
def parse_interval(interval_string):
    interval_string = interval_string.replace(" ", "")
    
    left_inclusive = interval_string[0] == '['
    right_inclusive = interval_string[-1] == ']'
    
    values = interval_string[1:-1].split(',')
    
    left = sympify(values[0])
    right = sympify(values[1])
    
    return srepr(Interval(left, right, left_open=not left_inclusive, right_open=not right_inclusive))
  
  
def parse_sets(set_string):     
    parts = split_by_separators(set_string, extract_notations(SET_NOTATIONS))

    parsed_sets = []
    for part in parts:
        if r'\mathbb' in part:
            parsed_sets.append(f'Mathbb({srepr(Symbol(extract_string_between_braces(part.strip())))})')
        elif has_interval(part.strip()):
            parsed_sets.append(parse_interval(part.strip()))
        else: 
            parsed_sets.append(srepr(parse_latex(part.strip())))

    set_relations = []
    
    index = 0
    for separator in re.findall('|'.join([re.escape(rel) for rel in extract_notations(SET_NOTATIONS)]), set_string):
        if separator == get_notation('In', SET_NOTATIONS):
            set_relations.append(f'In({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('ProperSubset', SET_NOTATIONS):
            set_relations.append(f'ProperSubset({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('Subset', SET_NOTATIONS):
            set_relations.append(f'Subset({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('NotIn', SET_NOTATIONS):
            set_relations.append(f'NotIn({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('ProperSupset', SET_NOTATIONS):
            set_relations.append(f'ProperSupset({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('Supset', SET_NOTATIONS):
            set_relations.append(f'Supset({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('Union', SET_NOTATIONS):
            set_relations.append(f'Union({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('Intersection', SET_NOTATIONS):
            set_relations.append(f'Intersection({parsed_sets[index]}, {parsed_sets[index + 1]})')
        elif separator == get_notation('Difference', SET_NOTATIONS):
            set_relations.append(f'Difference({parsed_sets[index]}, {parsed_sets[index + 1]})')
        index += 1    
    return set_relations
         

def parse_equalities(string):
    parts = split_by_separators(string, extract_notations(EQUALITY_NOTATIONS))
    equalities = []
    parsed = None
    for part in parts:
        if r'\mathbb' in part:
            parsed = parse_math_set(part)
        if has_inequality_relation(part.strip()):
            parsed = parse_inequalities(part.strip())
        elif has_set_relation(part.strip()):
            parsed = parse_sets(part.strip())  
        else: 
            parsed = srepr(parse_latex(part))
            
        if (isinstance(parsed, list)):
            parsed = parsed[0]
            
        equalities.append(parsed)   
            
    equality_relations = []
    index = 0
    
    for separator in re.findall('|'.join([re.escape(rel) for rel in extract_notations(EQUALITY_NOTATIONS)]), string):
        if separator == get_notation('Equality', EQUALITY_NOTATIONS):          
            equality_relations.append(f'Equality({equalities[index]}, {equalities[index+1]})')
        index += 1 

    return equality_relations 

def split_and_filter_non_empty(string):
    return [line.strip() for line in string.split('\n') if line.strip()]


def parse_math_set(str: str):
    idx = str.find(MATH_SET_CONTAINER) + len(MATH_SET_CONTAINER)

    stripped = str[idx:].strip()
    formatted = stripped
    if stripped.startswith("{") and stripped.endswith("}"):
        
        formatted = extract_string_between_braces(stripped)
        
    return f'Mathbb({srepr(Symbol(formatted))})'