RELATIONS = [ '<', '>', r'\leq', r'\geq', r'\leqq', r'\leqslant', r'\lesssim',
              r'\lessapprox', r'\prec', r'\preceq', r'\lessgtr', r'\lesseqgtr',
              r'\geq', r'\geqq', r'\geqslant', r'\gtrsim', r'\gtrapprox', r'\succ', 
              r'\succeq', r'\succsim', r'\nless', r'\ngtr', r'\nleq', r'\ngeq',
              r'\lneq', r'\gneq', r'\nleqslant', r'\ngeqslant', r'\ll', r'\lll', r'\gg', r'\ggg']

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
    