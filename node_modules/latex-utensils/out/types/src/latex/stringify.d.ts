import * as lp from './latex_parser_types';
/**
 * Converts nodes to a LaTeX string.
 * @param node Nodes to be converted to a LaTeX string.
 * @param options
 */
export declare function stringify(node: lp.Node | lp.Node[], options?: {
    lineBreak: string;
}): string;
