import turkishnlp
from turkishnlp import detector
import urllib.request

#opener = urllib.request.build_opener()
#opener.addheaders = [('User-Agent', 'MyApp/1.0')]
#urllib.request.install_opener(opener)

obj = detector.TurkishNLP()
#obj.download()

syllabified_sent = obj.syllabicate_sentence("Hiç unutmadım, doğudan esen hafif bir yel saçlarını dalgalandırıyordu")
syllabified_word = [syllable
                    for word in syllabified_sent
                    for syllable in word]

print(syllabified_word)
print(syllabified_sent)