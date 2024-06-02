from loquax import Document
from loquax.languages import Latin


text = "Quoūsque tandem abutēre, Catilīna, patientiā nostrā?"
syllable_count = 0
for token in Document(text, Latin).tokens:
    print(token, token.syllables, len(token.syllables))
    syllable_count += len(token.syllables)

print(syllable_count)




