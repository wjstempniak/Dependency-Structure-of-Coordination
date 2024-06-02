import csv
import os
import warnings
from languages import info
from io import open
from conllu import parse_incr
from counting_syllables import count_syllables
from time import perf_counter
from jamo import h2j  # korean blocks decomposition

maxInt = 2147483647
csv.field_size_limit(maxInt)

# Tree

def sort(sent, text):
    ids = []
    for word in text:
        ids.append(word["id"])
    sorted_ids = sorted(ids)

    sorted_text = []
    for id in sorted_ids:
        sorted_text.append(sent[id - 1])

    return sorted_text


def head(sent, word):
    if word["head"] > 0:
        return sent[word["head"] - 1]
    else:
        return ""


def children(sent, word):
    c = []
    for token in sent:
        if token["head"] == word['id']:
            c.append(token)
    return c


def desc(sent, word):
    d = []
    for child in children(sent, word):
        d.append(child)
        descendants = desc(sent, child)
        if descendants:
            for descendant in descendants:
                d.append(descendant)
    return sort(sent, d)


# Deprels

def identical_deprel(token1, token2, lang_params):
    if token1["deprel"] == token2["deprel"]:
        return True

    for e in lang_params["identical.deprels"]:
        if token1["deprel"] in e and token2["deprel"] in e:
            return True

    return False


# Continuity

def is_between(conjunct, token):
    tokens_before = tokens_after = False
    if token in conjunct:
        return False
    for t in conjunct:
        if t['id'] < token['id']:
            tokens_before = True
        if t['id'] > token['id']:
            tokens_after = True
    return tokens_before and tokens_after


# Table

def text(c, lang_params):
    txt = ""
    quot_open = False
    for i in range(len(c)):

        word = c[i]
        prev_word = c[i - 1]

        if i != 0:
            if word['form'] in '.,:;)?!”«' or prev_word['form'] in "(„»" \
                    or quot_open and (prev_word['form'] == '"' or word['form'] == '"') \
                    or word["deprel"] in lang_params["affixes"]:
                pass
            else:
                txt += " "
        txt += word['form']
        if word['form'] == '"':
            quot_open = not quot_open
    return txt


def tags(token):
    word = ""
    tag = ""
    pos = ""
    ms = ""
    if token:
        word = token["form"]
        tag = token["xpos"]
        pos = token["upos"]
        if token["feats"] != {'None': ''}:
            ms = token["feats"]
    return [word, tag, pos, ms]


def stats(conjunct, lang_params):
    words = 0
    for token in conjunct:
        words += (token["upos"] not in lang_params["non.words"])
    tokens = len(conjunct)
    conjunct_text = text(conjunct, lang_params)
    if lang_params['language'] != "Korean":
        syllables = count_syllables(conjunct_text, lang_params["language"])
        chars = len(conjunct_text)
    else:
        syllables = count_syllables(conjunct_text, lang_params["language"])
        chars = len(h2j(conjunct_text))
    return [words, tokens, syllables, chars]


# Finding and extracting coordination

def is_Lhead(sent, word):
    for c in children(sent, word):
        if c["deprel"] == "conj":
            return True
    return False


def conjuncts(sent, heads, lang_params):
    # heuristics = lang_params['heuristics']
    heuristics = ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6']
    heuristics_used = []
    Lhead = heads[0]
    Rhead = heads[-1]

    puncts = ",;:-"

    # R conjunct

    Rdesc = desc(sent, Rhead)
    to_remove = []

    for token in Rdesc:

        # H1 - PUNCTUATION
        if "H1" in heuristics:
            if token == Rdesc[0] and token['form'] in puncts:
                # print("H1", token)
                heuristics_used.append("H1")
                to_remove.append(token)

        # H2 - CONJUNTION
        if "H2" in heuristics:
            if token in children(sent, Rhead) and token['deprel'] == 'cc':
                # print("H2", token)
                heuristics_used.append("H2")
                to_remove.append(token)

    # Removing
    for t in to_remove:
        if t in Rdesc:
            Rdesc.remove(t)

    Rconj = Rdesc + [Rhead]

    # L conjunct

    Ldesc = desc(sent, Lhead)
    to_remove = []

    for token in Ldesc:

        # H0 - HEADS
        if "H0" in heuristics:
            if token in heads:
                to_remove.append(token)

        # H1 - PUNCTUATION
        if "H1" in heuristics:
            if token == Ldesc[0] and token['form'] in puncts:
                # print("H1", token)
                heuristics_used.append("H1")
                to_remove.append(token)

        # H2 - CONJUNTION
        if "H2" in heuristics:
            if token in children(sent, Lhead) and token['deprel'] == 'cc':
                # print("H2", token)
                heuristics_used.append("H2")
                to_remove.append(token)

        # H3 - Lhead children after Rconj
        if "H3" in heuristics:
            if token["id"] > Rconj[-1]["id"]:
                # print("H3", token)
                heuristics_used.append("H3")
                to_remove.append(token)

        # H5 - Common child before Lhead
        if "H5" in heuristics:
            if token["id"] < Lhead["id"] and token["head"] == Lhead["id"]:
                unique = True
                for h in heads:
                    if h == Lhead:
                        continue
                    for child in children(sent, h):
                        if child != token and identical_deprel(child, token, lang_params):
                            unique = False
                if unique:
                    # print("H5", token)
                    heuristics_used.append("H5")
                    to_remove.append(token)

    # Removing
    for t in to_remove:
        if t in Ldesc:
            Ldesc.remove(t)
        for d in desc(sent, t):
            if d in Ldesc:
                Ldesc.remove(d)

    Lconj = Ldesc + [Lhead]

    # H6 - CONTINUITY
    if "H6" in heuristics:
        for token in sent:
            if is_between(Lconj, token):
                heuristics_used.append("H6")
                # print(f'{sent.metadata["text"]} \nH6: {str(token)}')
                Lconj.append(token)

    Lconj = sort(sent, Lconj)

    # R conjunct again

    to_remove = []

    for token in Rconj:
        # H4 - Rhead children before Lconj
        if "H4" in heuristics:
            if token["id"] < Lconj[0]["id"]:
                # print("H4", token)
                heuristics_used.append("H4")
                to_remove.append(token)

    # Removing
    for t in to_remove:
        if t in Rconj:
            Rconj.remove(t)

    # H6 - CONTINUITY
    if "H6" in heuristics:
        for token in sent:
            if is_between(Rconj, token):
                heuristics_used.append("H6")
                # print(f'{sent.metadata["text"]} \nH6: {str(token)}')
                Rconj.append(token)

    Rconj = sort(sent, Rconj)

    heuristics_used = list(set(heuristics_used))

    return [Lconj, Rconj, heuristics_used]


def coordination(sent, heads, lang_params, corpus, is_nested):
    # Heads
    Lhead = heads[0]
    Rhead = heads[-1]

    # Conjunction
    C = ''
    for c in children(sent, Rhead):
        if c["deprel"] == "cc":
            C = c

    # Conjuncts
    conj = conjuncts(sent, heads, lang_params)
    L = conj[0]
    R = conj[1]

    heuristics_used = conj[2]
    if is_nested:
        heuristics_used.append("H7")

    # Governor
    G = head(sent, Lhead)
    gov_position = 0
    if G != "":
        if G["id"] < L[0]["id"]:
            gov_position = "L"
        elif G["id"] > R[-1]["id"]:
            gov_position = "R"
        else:
            gov_position = "M"

    if "" in heuristics_used:
        print(f'({gov_position}) [{text(L, lang_params)}] {C} [{text(R, lang_params)}]')

    coord_data = [gov_position] + tags(G) + tags(C) + [len(heads)] + \
                 [text(L, lang_params)] + [Lhead["deprel"]] + tags(Lhead) + stats(L, lang_params) + \
                 [text(R, lang_params)] + [Rhead["deprel"]] + tags(Rhead) + stats(R, lang_params) + \
                 [heuristics_used] + \
                 [lang_params['language'], corpus, sent.metadata['sent_id'], sent.metadata['text']]

    return coord_data


def coordinations(sent, Lhead, lang_params, corpus):
    # Heads
    heads = [Lhead]
    for c in children(sent, Lhead):
        if c["deprel"] == "conj":
            heads.append(c)

    # Conjunction
    Rhead = heads[-1]
    C = ""
    for c in children(sent, Rhead):
        if c["deprel"] == "cc":
            C = c

    # Nested coordinations with different conjunctions
    if C and len(heads) > 2:
        h = 0
        for head in heads:
            if h == 0:
                h += 1
                continue
            h += 1
            for c in children(sent, head):
                if c["deprel"] == "cc" and c["form"] != C["form"]:  # A & B / C
                    coords = []
                    coords += [coordination(sent, heads[0:h],
                                            lang_params, corpus, is_nested=True)]  # [A] & [B]
                    coords += [coordination(sent, [heads[0]] + heads[h:],
                                            lang_params, corpus, is_nested=True)]  # [A & B] / [C]
                    return coords

    return [coordination(sent, heads, lang_params, corpus, is_nested=False)]


# Analysing files

def analyse(input, output, lang_params, corpus):
    with open(input, "r", encoding="utf-8") as infile, \
            open(output, "w", encoding="utf-8", newline='') as outfile:

        # Header
        writer = csv.writer(outfile)
        header = ["governor.position", "governor.word", "governor.tag", "governor.pos", "governor.ms",
                  "conjunction.word", "conjunction.tag", "conjunction.pos", "conjunction.ms",
                  "no.conjuncts", "L.conjunct", "L.dep.label", "L.head.word", "L.head.tag",
                  "L.head.pos", "L.head.ms", "L.words", "L.tokens", "L.syllables", "L.chars",
                  "R.conjunct", "R.dep.label", "R.head.word", "R.head.tag", "R.head.pos", "R.head.ms",
                  "R.words", "R.tokens", "R.syllables", "R.chars",
                  "heuristics.used",
                  "language", "corpus", "sent.id", "text"]
        writer.writerow(header)

        sent_count = 0
        coord_count = 0

        for sent in parse_incr(infile):
            sent_count += 1

            # Multi-word tokens
            for token in sent.copy():
                if not isinstance(token['id'], int):
                    sent.remove(token)

            # Coordination
            for token in sent:
                if is_Lhead(sent, token):  # Find
                    for coord in coordinations(sent, token, lang_params, corpus):  # Extract
                        coord_count += 1
                        writer.writerow(coord)

        print(f'{sent_count} sentences analysed. \n'
              f'{coord_count} coordinations found.')


def main(folder, languages=None):
    if languages is None:
        languages = []

    for language in os.listdir(folder):

        if languages:
            if language not in languages:
                continue

        input_folder = folder + '/' + language + '/input'
        output_folder = folder + '/' + language + '/output'

        for file in os.listdir(input_folder):
            input = input_folder + '/' + file
            output = output_folder + '/' + file.split('.')[0] + ".csv"
            corpus = file.split(sep="-")[0].upper()
            print(f'\nLanguage: {language}'
                  f'\nCorpus: {corpus}'
                  f'\nFile: {input}')

            start_time = perf_counter()
            analyse(input, output, info[language], corpus)
            end_time = perf_counter()
            time = int(end_time-start_time)
            print(f'Time: {time} s.')


main("C:/Users/wojte/OneDrive/Documents/Dependency Structure of Coordination/corpora",
     languages=None) # None = all languages
