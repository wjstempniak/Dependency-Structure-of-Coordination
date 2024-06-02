from jamo import h2j

text = "구매자는 판매자에게 제품 대금으로 20달러를 지급하여야 한다."

words = text.split()

chars = 0
for word in words:
    jamos = h2j(word)
    chars += len(jamos)

print(len(h2j(text)))
print(chars)
