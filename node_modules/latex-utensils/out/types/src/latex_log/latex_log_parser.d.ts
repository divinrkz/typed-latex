import * as llp from './latex_log_types';
import { ParserOptions } from '../pegjs/pegjs_types';
export * from './latex_log_types';
export { isSyntaxError } from '../pegjs/pegjs_types';
export type { ParserOptions, SyntaxError } from '../pegjs/pegjs_types';
export declare function parse(s: string, optArg?: ParserOptions): llp.LatexLogAst;
