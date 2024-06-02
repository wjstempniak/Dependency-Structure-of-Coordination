import trankit
import pyperclip

def sent2latex(sentence, language):
    parser = trankit.Pipeline(language, embedding='xlm-roberta-large')

    # Ensure that the input contains only one sentence
    assert len(parser(sentence)['sentences']) == 1, "Input text should contain exactly one sentence."

    parsed = parser(sentence)['sentences'][0]

    # Retrieve token information
    tokens = parsed['tokens']

    # Create LaTeX code for dependency tree
    latex_code = "\\begin{exe} \n" \
                 "\\ex\n" \
                 "\\begin{dependency}[baseline=-\\the\\dimexpr\\fontdimen22\\textfont2\\relax]\n" \
                 "\\begin{deptext}[column sep=1em]\n"

    # Remove multiword tokens
    single_word_tokens = []
    for token in tokens:
        if isinstance(token['id'], int):
            single_word_tokens.append(token)
        else:
            for single_word_token in token['expanded']:
                single_word_tokens.append(single_word_token)

    # Add words
    for token in single_word_tokens:
        latex_code += token['text'] + " \\& "

    latex_code = latex_code[:-3]  # remove the last '&'
    latex_code += " \\\\ \n"
    latex_code += "\\end{deptext}\n"

    # Add dependency relations
    for token in single_word_tokens:
        if token['head'] == 0:
            latex_code += f"\\deproot{{{token['id']}}}{{root}}\n"
        else:
            latex_code += f"\\depedge{{{token['head']}}}{{{token['id']}}}{{{token['deprel']}}}\n"

    latex_code += "\\end{dependency}\n" \
                  "\\end{exe}\n"

    return latex_code

pyperclip.copy(sent2latex(
    sentence = "Chcą tworzyć, konstruować.",
    language = "polish"))