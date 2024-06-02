info = {
    "Arabic":  # no syllabication
        {'language': 'Arabic',
         'code': 'ar',
         'long.code': 'ar',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Czech":
        {'language': 'Czech',
         'code': 'cz',
         'long.code': 'cs_CZ',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "English":
        {'language': 'English',
         'code': 'en',
         'long.code': 'en',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "German":
        {'language': 'German',
         'code': 'de',
         'long.code': 'de',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Icelandic":
        {'language': 'Icelandic',
         'code': 'is',
         'long.code': 'is',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Italian":
        {'language': 'Italian',
         'code': 'it',
         'long.code': 'it_IT',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Korean":  # one syllable per block
        {'language': 'Korean',
         'code': 'ko',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},

    "Latin":  # https://pypi.org/project/loquax/
        {'language': 'Latin',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Polish":
        {'language': 'Polish',
         'code': 'pl',  # num2words code
         'long.code': 'pl_PL',  # pyphen code
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': ["aux:clitic"],
         'brev.pun': {'A': 'accusativus', 'a.s.c': 'akta_stanu_cywilnego', 'a': 'albo', 'al': 'aleja',
                      'ang': 'angielski',
                      'A.D': 'Anno_Domini', 'Apok': 'Apokalipsa_świętego_Jana', 'arch': 'architekt',
                      'arkt': 'arktyczny',
                      'b': 'bieżący', 'bdb': 'bardzo_dobry', 'b.u': 'bez_uwag', 'b.zm': 'bez_zmian', 'B': 'biernik',
                      'bm': 'bieżący_miesiąc', 'br': 'bieżący_rok', 'błp': 'błogosławionej_pamięci',
                      'bp': 'błogosławionej_pamięci', 'bł': 'błogosławiony', 'C': 'cieśnina', 'cd': 'ciąg_dalszy',
                      'cdn': 'ciąg_dalszy_nastąpi', 'Co': 'company', 'Corp': 'corporation', 'c': 'córka', 'cz': 'część',
                      'czyt': 'czytaj', 'dcn': 'dalszy_ciąg_nastąpi', 'D': 'dativus', 'd': 'dzień', 'dpt': 'dioptria',
                      'dł': 'długość', 'db': 'dobry', 'doc': 'docent', 'dol': 'dolar', 'dom': 'domowy',
                      'ds': 'do_spraw',
                      'dst': 'dostateczny', 'dot': 'dotyczy', 'dyr': 'dyrektor', 'dz': 'dziennik',
                      'Dz.U': 'Dziennik_Ustaw',
                      'dn': 'dzień', 'egz': 'egzemplarz', 'etc': 'et_cetera', 'ew': 'ewentualnie', 'f': 'femininum',
                      'fot': 'fotografia', 'gen': 'generał', 'Gen': 'Genesis', 'G': 'genetivus', 'głęb': 'głębokość',
                      'gm': 'gmina', 'godz': 'godzina', 'g': 'godzina', 'gr': 'grupa', 'hab': 'habilitowany',
                      'hm': 'harcmistrz',
                      'h.c': 'honoris_causa', 'hr': 'hrabia', 'ib': 'ibidem', 'ibid': 'ibidem', 'im': 'imienia',
                      'in': 'inaczej',
                      'ie': 'indoeuropejski', 'I': 'instrumentalis', 'itd': 'i_tak_dalej', 'itp': 'i_tym_podobne',
                      'jw': 'jak_wyżej', 'J': 'jezioro', 'jez': 'jezioro', 'jr': 'junior', 'jun': 'junior',
                      'kal': 'kaloria',
                      'kpt': 'kapitan', 'kpr': 'kapral', 'kat': 'katalog', 'kl': 'klasa', 'k': 'kodeks',
                      'kol': 'koleżanka',
                      'x': 'książę', 'ks': 'ksiądz', 'Jer': 'Księga_Jeremiasza', 'Joz': 'Księga_Jozuego',
                      'Kazn': 'Księga_Kaznodziei', 'Kron': 'Księga_Kronik', 'Pr': 'Księga_Przysłów', 'kw': 'kwadratowy',
                      'l': 'lata', 'lm': 'liczba_mnoga', 'lp': 'liczba_pojedyncza', 'Ltd': 'limited',
                      'Hebr': 'List_do_Hebrajczyków', 'lit': 'literatura', 'L': 'locativus', 'łac': 'łaciński',
                      'mar': 'marynarz',
                      'mec': 'mecenas', 'm': 'morze', 'M': 'mianownik', 'Msc': 'miejscownik', 'Ms': 'miejscownik',
                      'mrn': 'mierny', 'mk': 'mieszkaniec', 'mieszk': 'mieszkaniec', 'm.in': 'między_innymi',
                      'min': 'minimum',
                      'n': 'następny', 'n.p.m': 'nad_poziomem_morza', 'nzw': 'nadzwyczajny', 'np': 'na_przykład',
                      'N': 'nominativus', 'nn': 'następne', 'n.e': 'naszej_ery', 'nt': 'na_temat',
                      'ndst': 'niedostateczny',
                      'nlb': 'nieliczbowany', 'ndm': 'nieodmienny', 'N.N': 'nomen_nescio', 'nb': 'nota_bene',
                      'Obj': 'Objawienie_świętego_Jana', 'ob': 'obywatel', 'ogp': 'ogólnopolski', 'o': 'ojciec',
                      'oo': 'ojcowie',
                      'ok': 'około', 'op.cit': 'opus_citatum', 'os': 'osiedle', 'p': 'pan', 'pp': 'panie',
                      'paraf': 'parafraza',
                      'p.o': 'pełniący_obowiązki', 'P.n.P': 'Pieśń_nad_Pieśniami', 'pl': 'plac', 'P.T': 'pleno_titulo',
                      'pocz': 'początek', 'pchor': 'podchorąży', 'ppor': 'podporucznik', 'p.p.m': 'pod_poziomem_morza',
                      'ppł': 'podpułkownik', 'pt': 'pod_tytułem', 'pw': 'pod_wezwaniem', 'pol': 'polski',
                      'poł': 'połowa',
                      'pd': 'południe', 'płd': 'południowy', 'pn': 'północ', 'por': 'porównaj', 'ps': 'pseudonim',
                      'pow': 'powierzchnia', 'poz': 'pozycja', 'płn': 'północny', 'proc': 'procent', 'prof': 'profesor',
                      'proj': 'projekt', 'p.n.e': 'przed_naszą_erą', 'przyp': 'przypis', 'red': 'redakcja',
                      'rtg': 'rentgen',
                      'reż': 'reżyseria', 'r': 'rodzaj', 'R.P': 'Roku_Pańskiego', 'rtm': 'rotmistrz',
                      'rozdz': 'rozdział',
                      'ryc': 'rycina', 'rys': 'rysunek', 'rz': 'rzeka', 's.f': 'science_fiction', 'sek': 'sekunda',
                      'sen': 'senior', 'sr': 'senior', 's': 'siostra', 'ss': 'siostry', 'społ': 'społeczeństwo',
                      'sp': 'spółka',
                      's.c': 'spółka_cywilna', 'st': 'staro', 'stp': 'staropolski', 'str': 'strona', 'Sz': 'Szanowny',
                      'szer': 'szeregowy', 'szt': 'sztuka', 'św': 'świadek', 'śś': 'święci', 'śp': 'świętej_pamięci',
                      'tab': 'tabela', 'tzw': 'tak_zwany', 'tel': 'telefon', 'temp': 'temperatura', 'tł': 'tłumaczył',
                      'tj': 'to_jest', 't': 'tom', 'tow': 'towarzystwo', 'tzn': 'to_znaczy', 'tys': 'tysiąc',
                      'ub': 'ubiegły',
                      'ul': 'ulica', 'ur': 'urodzony', 'U': 'ustawa', 'ust': 'ustęp', 'v': 'verte', 'V': 'vocativus',
                      'vol': 'volumen', 'w': 'wyspa', 'Wlk': 'Wielki', 'W.Ks.Lit': 'Wielkie_Księstwo_Litewskie',
                      'W': 'Wielmożny',
                      'ww': 'wyżej_wymieniony', 'wł': 'włącznie', 'woj': 'województwo', 'wsch': 'wschodni',
                      'ws': 'w_sprawie',
                      'wyd': 'wydanie', 'wz': 'wzorowy', 'zach': 'zachodni', 'zam': 'zamieszkały', 'z': 'zeszyt',
                      'zm': 'zmarł',
                      'zob': 'zobacz', 'ż': 'żeński'},
         'brev.npun': {'Ap': 'Apokalipsa_świętego_Jana', 'ASAP': 'as_soon_as_possible', 'B': 'bajt',
                       'blm': 'bez_liczby_mnogiej',
                       'blp': 'bez_liczby_pojedynczej', 'bld': 'biliard', 'bln': 'bilion', 'b': 'bit',
                       'bps': 'bit_per_second',
                       'BTW': 'by_the_way', 'cal': 'caloria', 'c': 'cent', 'cm': 'centymetr', 'ca': 'circa',
                       'ccm': 'cubic_centimetre', 'dl': 'decylitr', 'dkg': 'dekagram', 'dag': 'dekagram',
                       'DEM': 'Deutsche_Mark',
                       'dk': 'dokonany', 'dr': 'doktor', 'USD': 'dolar_amerykański', 'Dz': 'Dzieje', 'EUR': 'euro',
                       'J': 'Ewangelia_świętego_Jana', 'Jan': 'Ewangelia_świętego_Jana',
                       'Mk': 'Ewangelia_świętego_Marka',
                       'Mt': 'Ewangelia_świętego_Mateusza', 'Łk': 'Ewangelia_wg_świętego_Łukasza', 'fb': 'Facebook',
                       'GW': 'Gazeta_Wyborcza', 'GB': 'gigabajt', 'GHz': 'gigaherc', 'h': 'godzina', 'g': 'gram',
                       'gr': 'grosz',
                       'ha': 'hektar', 'hl': 'hektolitr', 'Hz': 'herc', 'IMHO': 'in_my_humble_opinion',
                       'IMO': 'in_my_opinion',
                       'JWP': 'Jaśnie_Wielmożna_Pani', 'kB': 'kilobajt', 'kg': 'kilogram', 'kHz': 'kiloherc',
                       'kcal': 'kilokaloria', 'km': 'kilometr', 'kW': 'kilowat', 'kWh': 'kilowatogodzina',
                       'k': 'kompania',
                       'Ab': 'Księga_Abdiasza', 'Ag': 'Księga_Aggeusza', 'Am': 'Księga_Amosa', 'Ba': 'Księga_Barucha',
                       'Dn': 'Księga_Daniela', 'Est': 'Księga_Estery', 'Ezd': 'Księga_Ezdrasza',
                       'Ez': 'Księga_Ezechiela',
                       'Eze': 'Księga_Ezechiela', 'Ha': 'Księga_Habakuka', 'Hab': 'Księga_Habakuka',
                       'Hi': 'Księga_Hioba',
                       'Iz': 'Księga_Izajasza', 'Jr': 'Księga_Jeremiasza', 'Job': 'Księga_Joba', 'Jl': 'Księga_Joela',
                       'Jon': 'Księga_Jonasza', 'Jdt': 'Księga_Judyty', 'Kpł': 'Księga_Kapłańska',
                       'Koh': 'Księga_Koheleta',
                       'Krn': 'Księga_Kronik', 'Krl': 'Księga_Królów', 'Kl': 'Księga_Królów', 'Lm': 'Księga_Lamentacje',
                       'Lam': 'Księga_Lamentacje', 'Lb': 'Księga_Liczb', 'Mch': 'Księga_Machabejska',
                       'Ml': 'Księga_Malachiasza',
                       'Mal': 'Księga_Malachiasza', 'Mdr': 'Księga_Mądrości', 'Mi': 'Księga_Micheasza',
                       'Nah': 'Księga_Nahuma',
                       'Ne': 'Księga_Nehemiasza', 'Neh': 'Księga_Nehemiasza', 'Oz': 'Księga_Ozeasza',
                       'Pwt': 'Księga_Powtórzonego_Prawa', 'Prz': 'Księga_Przysłów', 'Ps': 'Księga_Psalmów',
                       'Rdz': 'Księga_Rodzaju', 'Rut': 'Księga_Rut', 'Sm': 'Księga_Samuela', 'Sdz': 'Księga_Sędziów',
                       'So': 'Księga_Sofoniasza', 'Sof': 'Księga_Sofoniasza', 'Syr': 'Księga_Syracha',
                       'Wj': 'Księga_Wyjścia',
                       'Ef': 'List_do_Efezjan', 'Flm': 'List_do_Filemona', 'Flp': 'List_do_Filipian',
                       'Ga': 'List_do_Galatów',
                       'Gal': 'List_do_Galatów', 'Hbr': 'List_do_Hebrajczyków', 'Kol': 'List_do_Kolosan',
                       'Kor': 'List_do_Koryntian', 'Ko': 'List_do_Koryntian', 'Rz': 'List_do_Rzymian',
                       'Tes': 'List_do_Tesaloniczan', 'Tm': 'List_do_Tymoteusza', 'Tt': 'List_do_Tytusa',
                       'Jk': 'List_świętego_Jakuba', 'Jud': 'List_świętego_Judy', 'P': 'List_świętego_Piotra',
                       'Pt': 'List_świętego_Piotra', 'l': 'litr', 'M-me': 'Madame', 'MB': 'Matka_Boska',
                       'MHz': 'megaherc',
                       'm': 'metr', 'mld': 'miliard', 'mg': 'miligram', 'mgr': 'magister', 'ml': 'mililitr',
                       'mm': 'milimetr',
                       'mln': 'milion', 'min': 'minuta', 'Mr': 'Mister', 'Mrs': 'Mistress', 'm/s': 'motor_ship',
                       'm/y': 'motor_yacht', 'NMP': 'Najświętsza_Maryja_Panna', 'ndk': 'niedokonany',
                       'PLN': 'nowy_polski_złoty',
                       'NT': 'Nowy_Testament', 'n/s': 'nuclear_ship', 'nr': 'numer', 'pt': 'piątek',
                       'PP': 'Poczta_Polska',
                       'ppłk': 'podpułkownik', 'PL': 'Polska', 'PS': 'post_scriptum', 'płk': 'pułkownik',
                       'pkt': 'punkt',
                       's/y': 'sail_yacht', 's': 'sekunda', 'ST': 'Stary_Testament', 's/s': 'steam_ship',
                       'SP': 'Szkoła_Podstawowa', 'tlx': 'telegraph_exchange', 't': 'tona', 'TT': 'Twitter',
                       'vs': 'versus',
                       'wg': 'według', 'WXL': 'Wielkie_Księstwo_Litewskie', 'WXLit': 'Wielkie_Księstwo_Litewskie',
                       'zł': 'złoty',
                       '$': 'dolarów', '€': 'euro'}
         },
    "Portuguese":
        {'language': 'Portuguese',
         'code': 'pt',
         'long.code': 'pt_PT',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Romanian":
        {'language': 'Romanian',
         'code': 'ro',
         'long.code': 'ro',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Russian":
        {'language': 'Russian',
         'code': 'ru',
         'long.code': 'ru_RU',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Spanish":
        {'language': 'Spanish',
         'code': 'es',
         'long.code': 'es_ES',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []},
    "Turkish":  # https://pypi.org/project/turkishnlp/
        {'language': 'Turkish',
         'code': 'tr',
         'heuristics': ['H0', 'H1', 'H2', 'H3', 'H4', 'H5', 'H6'],
         'identical.deprels': [["subj", "subj:pass"], ["nummod", "nummod:gov"]],
         'non.words': ["PUNCT"],
         'affixes': []}
}
