import csv
import os
from io import open
from conllu import parse_incr
from counting_syllables import count_syllables

maxInt = 2147483647
csv.field_size_limit(maxInt)


### Tree

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


### Deprels

def identical_deprel(token1, token2):
    exceptions = [
        ["subj", "subj:pass"],
        ["nummod", "nummod:gov"]
    ]

    if token1["deprel"] == token2["deprel"]:
        return True

    for e in exceptions:
        if token1["deprel"] in e and token2["deprel"] in e:
            return True

    return False


### Table

def text(c):
    txt = ""
    quot_open = False
    for i in range(len(c)):

        word = c[i]
        prev_word = c[i - 1]

        if i != 0:
            if word['form'] in '.,:;)?!”' or prev_word['form'] in "(„" \
                    or quot_open and (prev_word['form'] == '"' or word['form'] == '"') \
                    or word["deprel"] in ["aux:clitic"]:
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


def stats(c):
    words = 0
    for token in c:
        words += (token["upos"] not in ["PUNCT", "aux:clitic"])  # aux:clitic == -em, -śmy, itd.
    tokens = len(c)
    c_text = text(c)
    syllables = count_syllables(c_text)
    chars = len(c_text)
    return [words, tokens, syllables, chars]


### Finding and extracting coordination

def is_Lhead(sent, word):
    for c in children(sent, word):
        if c["deprel"] == "conj":
            return True
    return False


def conjuncts(sent, heads):
    Lhead = heads[0]
    Rhead = heads[-1]

    puncts = ",;:-"

    # R conjunct

    Rdesc = desc(sent, Rhead)
    to_remove = []

    for token in Rdesc:

        # H1 - PUNCTUATION
        if token == Rdesc[0] and token['form'] in puncts:
            # print("H1", token)
            h1 = True
            to_remove.append(token)

        # H2 - CONJUNTION
        if token in children(sent, Rhead) and token['deprel'] == 'cc':
            # print("H2", token)
            h2 = True
            to_remove.append(token)

    # Removing
    for t in to_remove:
        if t in Rdesc:
            Rdesc.remove(t)

    Rconj = sort(sent, Rdesc + [Rhead])

    # L conjunct

    Ldesc = desc(sent, Lhead)
    to_remove = []

    for token in Ldesc:

        # H1 - PUNCTUATION
        if token == Ldesc[0] and token['form'] in puncts:
            # print("H1", token)
            to_remove.append(token)

        # H2 - CONJUNTION
        if token in children(sent, Lhead) and token['deprel'] == 'cc':
            # print("H2", token)
            to_remove.append(token)

        # H3 - HEADS
        if token in heads:
            # print("H3", token)
            to_remove.append(token)

        # H4 - Lhead children after Rconj
        if token["id"] > Rconj[-1]["id"]:
            # print("H4", token)
            to_remove.append(token)

        # H5 - Common child before Lhead
        if token["id"] < Lhead["id"] and token["head"] == Lhead["id"]:
            unique = True
            for h in heads:
                if h == Lhead:
                    continue
                for child in children(sent, h):
                    if child != token and identical_deprel(child, token):
                        unique = False
            if unique:
                print("H5", token)
                to_remove.append(token)

    # Removing
    for t in to_remove:
        if t in Ldesc:
            Ldesc.remove(t)
        for d in desc(sent, t):
            if d in Ldesc:
                Ldesc.remove(d)

    Lconj = sort(sent, Ldesc + [Lhead])

    return [Lconj, Rconj]


def coordination(sent, heads):
    # Heads
    Lhead = heads[0]
    Rhead = heads[-1]

    # Conjunction
    C = ''
    for c in children(sent, Rhead):
        if c["deprel"] == "cc":
            C = c

            # Conjuncts
    conj = conjuncts(sent, heads)
    L = conj[0]
    R = conj[1]

    # Governor
    G = head(sent, Lhead)
    gov_position = 0
    if G != "":
        # noinspection PyTypeChecker
        if G["id"] < L[0]["id"]:
            gov_position = "L"
        elif G["id"] > R[-1]["id"]:
            gov_position = "R"
        else:
            gov_position = "M"


    print(f'({gov_position}) [{text(L)}] {C} [{text(R)}]')

    coord_data = [gov_position] + tags(G) + tags(C) + [len(heads)] + \
                 [text(L)] + [Lhead["deprel"]] + tags(Lhead) + stats(L) + \
                 [text(R)] + [Rhead["deprel"]] + tags(Rhead) + stats(R) + \
                 list(sent.metadata.values())

    return coord_data


def coordinations(sent, Lhead):
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
                    coords += [coordination(sent, heads[0:h])]  # [A] & [B]
                    coords += [coordination(sent, [heads[0]] + heads[h:])]  # [A & B] / [C]
                    return coords

    return [coordination(sent, heads)]


### Main

def main(input_folder, output_folder, end=0):
    sent_count = 0
    coord_count = 0

    for file in os.listdir(input_folder):
        input = input_folder + '/' + file
        output = output_folder + '/' + file.split('.')[0] + ".csv"

        print(f"\nFile: {input}")

        with open(input, "r", encoding="utf-8", errors='replace') as infile, \
                open(output, "w", encoding="utf-8", newline='') as outfile:

            writer = csv.writer(outfile)
            header = ["governor.position", "governor.word", "governor.tag", "governor.pos", "governor.ms",
                      "conjunction.word", "conjunction.tag", "conjunction.pos", "conjunction.ms",
                      "no.conjuncts", "L.conjunct", "L.dep.label", "L.head.word", "L.head.tag",
                      "L.head.pos", "L.head.ms", "L.words", "L.tokens", "L.syllables", "L.chars",
                      "R.conjunct", "R.dep.label", "R.head.word", "R.head.tag", "R.head.pos", "R.head.ms",
                      "R.words", "R.tokens", "R.syllables", "R.chars",
                      "id", "sent", "type", "channel", "file id", "para id", "directory"]  # Metadata
            writer.writerow(header)

            for sent in parse_incr(infile):

                # Coordination
                for word in sent:
                    if is_Lhead(sent, word):  # Find
                        for coord in coordinations(sent, word):  # Extract
                            coord_count += 1
                            writer.writerow(coord)

                if 0 < end < sent_count:
                    print(f'\n{coord_count} coords found in total.')
                    exit()


main(input_folder="path/to/folder/corpora/data_processing/test/conllu",
     output_folder="path/to/folder/corpora/data_processing/test/coordinations",
     end=0)
