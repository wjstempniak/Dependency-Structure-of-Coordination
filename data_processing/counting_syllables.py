import re
import pyphen
from turkishnlp import detector
from loquax import Document
from loquax.languages import Latin
from languages import info
from num2words import num2words as n


def syllables(word, language):

    syllabified_word = word
    syllable_count = 0

    try:
        brev_pun = info[language]["brev.pun"]
        brev_npun = info[language]["brev.npun"]
    except KeyError:
        brev_npun = brev_pun = {}

    word = re.sub("_", " ", word)
    word = re.sub('[,;:"(){}!@#^&*]', '', word)

    if word == "":  # punctuation-only strings
        return 0

    if word[:-1] in brev_pun and word[-1] == ".":  # Abbrevs with dot
        return syllables(brev_pun[word[:-1]], language)

    if word[-1] == ".":
        word = word[:-1]

    if word == "":
        return 0

    if word in brev_npun:  # Abbrevs without dot
        return syllables(brev_npun[word], language)

    for i in range(len(word), 0, -1):   # Numbers
        if word[0:i].isdigit():  # Numbers
            try:
                code = info[language]["code"]
                number = n(int(word[0:i]), lang=code)
                new_word = word[i:len(word)]
            except NotImplementedError:
                return 0
            if new_word:
                return count_syllables(number, language) + count_syllables(new_word, language)
            return count_syllables(number, language)

    try:
        long_code = info[language]["long.code"]
        syllabified_word = pyphen.Pyphen(lang=long_code).inserted(word.lower())
        syllable_count = syllabified_word.count('-') + 1  # Ordinary words

    except KeyError:
        if language == "Turkish":
            nlp = detector.TurkishNLP()
            syllabified_sent = nlp.syllabicate_sentence(word)
            syllabified_word = [syllable
                                for word in syllabified_sent
                                for syllable in word]
            syllable_count = len(syllabified_word)

        elif language == "Korean":
            syllable_count = len(word)  # that is why hangul is the greatest writing system in existence

        elif language == "Latin":
            for token in Document(word, Latin).tokens:
                syllable_count += len(token.syllables)

        else:
            return 0

    #print(syllable_count, syllabified_word) # for testing purposes
    return syllable_count


def count_syllables(text, language):
    try:
        text = text.split()
        syllable_count = 0

        for word in text:
            syllable_count += syllables(word, language)

        return syllable_count

    except KeyError:
        return 0
    except ValueError:
        return 0


PL_test = "Na ul. Głównej 15 pracują m.in. dr hab. Kowalski i mgr Nowak. " \
          "ITD wlepiła kierowcy BMW mandat 100 zł. " \
          "235 posłów PiS stanowi 51% Sejmu. " \
          "21 lipca 2014. " \
          "Księgi biblijne: Rdz, Wj, Lb, itd. .  " \
          "@#^(*&^ " \
          "PIJEMY DO DNA!"

DE_test = "Ich weiß nicht, was soll es bedeuten, daß ich so traurig bin. " \
          "Seit März 2001 sind 2852545 Artikel in deutscher Sprache entstanden."

KO_test = "구매자는 판매자에게 제품 대금으로 20달러를 지급하여야 한다."

TR_test = "21. Hiç unutmadım, doğudan esen hafif bir yel saçlarını dalgalandırıyordu."

LA_test = 'Pippini clementissimorum regum trigisimo tertio et vigisimo sexto, indictione XIIII.'

