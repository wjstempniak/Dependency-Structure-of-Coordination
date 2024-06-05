import trankit
import pyperclip

text = "Porządek i rozwój albo chaos i degeneracja."
language = "polish"

parse = trankit.Pipeline(language, embedding='xlm-roberta-large')

print(trankit.trankit2conllu(parse(text)))
