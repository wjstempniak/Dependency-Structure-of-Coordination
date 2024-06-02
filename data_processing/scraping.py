import csv
from urllib.request import urlopen
from bs4 import BeautifulSoup
import re

url = 'https://universaldependencies.org/'
html_content = urlopen(url).read().decode('utf-8')
soup = BeautifulSoup(html_content, 'html.parser')

headers_pattern = re.compile("ui-accordion-header ui-helper-reset ui-state-default ui-corner-all|"
                             "ui-accordion-content ui-helper-reset ui-widget-content ui-corner-bottom")
hub_pattern = re.compile("treebanks/.*/index.html")
token_pattern = re.compile('.* tokens .* words .* sentences')

headers = soup.find_all('div', class_=headers_pattern)
languages = []
treebanks = []
big_lang = False
language_group = ""

for header in headers:
    is_language = bool(header.find('img', class_='flag'))
    span = header.find('span', class_='doublewidespan')

    if span and is_language:
        name = span.text.strip()
        print(name)
        size = int(
            header.find('span', attrs={'data-hint': token_pattern}).attrs['data-hint'].split()[0].replace(',', ''))
        if size > 700000 or name in ["Polish", "Korean"]:
            languages.append(name)
            language_group = header.find_all('span', class_='triplewidespan')[-1].text.strip()
            big_lang = True
        else:
            big_lang = False
            continue

    if span and not is_language and big_lang:
        language = languages[-1]
        corpus = span.text.strip()
        size = int(
            header.find('span', attrs={'data-hint': token_pattern}).attrs['data-hint'].split()[0].replace(',', ''))

        tags = ""
        possible_tags = ["Lemmas", "Features", "Secondary dependencies"]
        for tag in possible_tags:
            if header.find('span', attrs={'data-hint': tag}):
                tags += tag
                tags += " "

        params = header.find_all('span', class_="hint--top hint--info")
        source = params[-3].attrs['data-hint']
        license_ = params[-2].attrs['data-hint']

        if size > 0:
            treebank = {"language": language, "corpus": corpus, "size": size,
                        "tags": tags, "source": source, "license": license_,
                        "language_group": language_group, "hub": "",
                        "Lemmas": "", "UPOS": "", "XPOS": "", "Features": "", "Relations": ""}
            treebanks.append(treebank)

    link = header.find('a', href=hub_pattern)
    if link and treebanks and big_lang:
        hub = "https://universaldependencies.org/" + link.attrs['href']
        treebanks[-1]['hub'] = hub

        try:
            hub_content = urlopen(hub).read().decode('utf-8')
        except:
            print(hub)
            continue
        hub_soup = BeautifulSoup(hub_content, 'html.parser')

        tables = hub_soup.find_all('table')
        if len(tables) > 1:
            table = tables[1]
            tds = table.find_all('td')
            treebanks[-1]['Lemmas'] = tds[1].text.strip()
            treebanks[-1]['UPOS'] = tds[3].text.strip()
            treebanks[-1]['XPOS'] = tds[5].text.strip()
            treebanks[-1]['Features'] = tds[7].text.strip()
            treebanks[-1]['Relations'] = tds[9].text.strip()

with open('treebanks.csv', 'w', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=list(treebanks[0].keys()))
    writer.writeheader()
    writer.writerows(treebanks)
