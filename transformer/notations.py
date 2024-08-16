MATH_SET_CONTAINER = r'\mathbb'

SUMMATION_REGEX = r'\\sum\s*_\s*\{[^}]+\}\s*\^\s*\{[^}]+\}'


    
SET_NOTATIONS = [
    {'name': 'In', 
     'notation': r'\in'
    },
    {'name': 'ProperSubset', 
     'notation': r'\subset'
    },
    {'name': 'Subset', 
     'notation': r'\subseteq'
    },
    {'name': 'NotIn', 
     'notation': r'\notin'
    },
    {'name': 'ProperSupset', 
     'notation': r'\supset'
    },
    {'name': 'Supset', 
     'notation': r'\supseteq'
    },
    {'name': 'Union', 
     'notation': r'\cup'
    },
    {'name': 'Intersection', 
     'notation': r'\cap'
    },
    {'name': 'Difference', 
     'notation': r'\setminus'
    }
]

INEQUALITY_NOTATIONS = [
    {'name': 'LessThan', 
     'notation': '<'
    },
    {'name': 'GreaterThan', 
     'notation': '>'
    },
    {'name': 'LessThanOrEqual', 
     'notation': r'\leq'
    },
    {'name': 'GreaterThanOrEqual', 
     'notation': r'\geq'
    },
    {'name': 'LessThanOrEqualEqual', 
     'notation': r'\leqq'
    },
    {'name': 'LessThanOrEqualSlant', 
     'notation': r'\leqslant'
    },
    {'name': 'LessSim', 
     'notation': r'\lesssim'
    },
    {'name': 'LessApprox', 
     'notation': r'\lessapprox'
    },
    {'name': 'Precedes', 
     'notation': r'\prec'
    },
    {'name': 'PrecedesEqual', 
     'notation': r'\preceq'
    },
    {'name': 'LessGreater', 
     'notation': r'\lessgtr'
    },
    {'name': 'LessEqualGreater', 
     'notation': r'\lesseqgtr'
    },
    {'name': 'GreaterThanOrEqual', 
     'notation': r'\geq'
    },
    {'name': 'GreaterEqualEqual', 
     'notation': r'\geqq'
    },
    {'name': 'GreaterThanOrEqualSlant', 
     'notation': r'\geqslant'
    },
    {'name': 'GreaterSim', 
     'notation': r'\gtrsim'
    },
    {'name': 'GreaterApprox', 
     'notation': r'\gtrapprox'
    },
    {'name': 'Succeeds', 
     'notation': r'\succ'
    },
    {'name': 'SucceedsEqual', 
     'notation': r'\succeq'
    },
    {'name': 'SucceedsSim', 
     'notation': r'\succsim'
    },
    {'name': 'NotLessThan', 
     'notation': r'\nless'
    },
    {'name': 'NotGreaterThan', 
     'notation': r'\ngtr'
    },
    {'name': 'NotLessThanOrEqual', 
     'notation': r'\nleq'
    },
    {'name': 'NotGreaterThanOrEqual', 
     'notation': r'\ngeq'
    },
    {'name': 'LessNotEqual', 
     'notation': r'\lneq'
    },
    {'name': 'GreaterNotEqual', 
     'notation': r'\gneq'
    },
    {'name': 'NotLessThanOrEqualSlant', 
     'notation': r'\nleqslant'
    },
    {'name': 'NotGreaterThanOrEqualSlant', 
     'notation': r'\ngeqslant'
    },
    {'name': 'MuchLessThan', 
     'notation': r'\ll'
    },
    {'name': 'VeryMuchLessThan', 
     'notation': r'\lll'
    },
    {'name': 'MuchGreaterThan', 
     'notation': r'\gg'
    },
    {'name': 'VeryMuchGreaterThan', 
     'notation': r'\ggg'
    }
]

EQUALITY_NOTATIONS = [
    {'name': 'Equality', 
     'notation': r'='
    },
    {'name': 'NotEqual', 
     'notation': r'\neq'
    },
    {'name': 'NotEqual', 
     'notation': r'\ne'
    },
    {'name': 'Approximate', 
     'notation': r'\approx'
    },
    {'name': 'Equilavence', 
     'notation': r'\iff'
    },
    {'name': 'LeftImplication', 
     'notation': r'\Leftarrow'
    },
    {'name': 'RightImplication', 
     'notation': r'\Rightarrow'
    },
]
