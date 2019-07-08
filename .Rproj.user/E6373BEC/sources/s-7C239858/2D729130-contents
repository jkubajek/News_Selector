# ###################################################################
# Polish stop words
# ###################################################################
stop_words_pl <- c("A", "a", "aby", "ach", "acz", "aczkolwiek", "aj", "albo", "ale", "Ale", "ależ", "ani", "aż", "bardziej", "bardzo", "bo", "bowiem", 
                   "by", "byli", "bynajmniej", "być", "był", "była", "było", "były", "będzie", "będą", "cali", "cała", "cały", "ci", "cię", 
                   "ciebie", "co", "cokolwiek", "coś", "czasami", "czasem", "czemu", "czy", "czyli", "daleko", "dla", "dlaczego", "dlatego", 
                   "do", "dobrze", "dokąd", "dość", "dużo", "dwa", "dwaj", "dwie", "dwoje", "dziś", "dzisiaj", "gdy", "gdyby", "gdyż", "gdzie", 
                   "gdziekolwiek", "gdzieś", "i", "ich", "ile", "im", "inna", "inne", "inny", "innych", "iż", "ja", "ją", "jak", "jaka", "jakaś", 
                   "jakby", "jaki", "jakichś", "jakie", "jakiś", "jakiż", "jakkolwiek", "jako", "jakoś", "je", "jeden", "jedna", "jedno", "jednak", 
                   "jednakże", "jego", "jej", "jemu", "jest", "jestem", "jeszcze", "jeśli", "jeżeli", "już", "ją", "każdy", "kiedy", "kilka", 
                   "kimś", "kto", "ktokolwiek", "ktoś", "która", "które", "którego", "której", "który", "których", "którym", "którzy", "ku", 
                   "lat", "lecz", "lub", "ma", "mają", "mało", "mam", "mi", "mimo", "między", "mną", "mnie", "mogą", "moi", "moim", "moja", "moje", 
                   "może", "możliwe", "można", "mój", "mu", "musi", "my", "na", "nad", "nam", "nami", "nas", "nasi", "nasz", "nasza", "nasze", 
                   "naszego", "naszych", "natomiast", "natychmiast", "nawet", "nią", "nic", "nich", "nie", "niech", "niego", "niej", "niemu", 
                   "nigdy", "nim", "nimi", "niż", "no", "o", "obok", "od", "około", "on", "ona", "one", "oni", "ono", "oraz", "oto", "owszem", 
                   "pan", "pana", "pani", "po", "pod", "podczas", "pomimo", "ponad", "ponieważ", "powinien", "powinna", "powinni", "powinno", "poza", 
                   "prawie", "przecież", "przed", "przede", "przedtem", "przez", "przy", "roku", "również", "sama", "są", "się", "skąd", "sobie", "sobą", 
                   "sposób", "swoje", "ta", "tak", "taka", "taki", "takie", "także", "tam", "te", "tego", "tej", "temu", "ten", "teraz", "też", "to", "tobą", 
                   "tobie", "toteż", "trzeba", "tu", "tutaj", "twoi", "twoim", "twoja", "twoje", "twym", "twój", "ty", "tych", "tylko", "tym", "u", "w", "wam", 
                   "wami", "was", "wasz", "wasza", "wasze", "we", "według", "wiele", "wielu", "więc", "więcej", "wszyscy", "wszystkich", "wszystkie", "wszystkim", 
                   "wszystko", "wtedy", "wy", "właśnie", "z", "za", "zapewne", "zawsze", "ze", "zł", "znowu", "znów", "został", "żaden", "żadna", "żadne", "żadnych", "że", "żeby",
                   "proc", "mln", "r", "tys", "PAP", "m.in", "min", "ok", "zaś", "np", "rdr", "p", "m", "tzw",
                   "go", "c", "br", "wśród", "I", "II", "III", "IV", "sas.cmd.push", "sas.render", "tę", "e", "fot", "ws", "kw", "art",
                   "tuż", "szt", "t", "tj", "bln", "mld", "godz", "of", "ów", "b", "12mies", "12M", "1M", "ad", "6M", "6d", "3M", "36M", "p.p", "p.a",
                   "function", "_mce_href", "mkw", "false", "left", "bottom", "padding", "hidden", "overflow", "relative", "unhidewhenused",
                   "1W", "1Y", "1D", "js", "pr", "bankier.pl", "yay", "pb.pl", "amp", "position", "embed", "container", "the", "https",
                   "name", "ytd", "pb", "YTD", "YAY", "KK", "PX", "BIST", "PX", "ASE", "BUX", "RTS", "KK", "DU", "WSZ", "LTV", "SNB", "BB", "XXI", "BLS",
                   "PCC", "POS", "XX", "CME", "IHS", "V", "VI", "I", "II", "III", "Iv", "VII", "VIII", "IX", "X", "XI", "XII", "CDR", "PDM", "AB",
                   "Torchała", "Hajdamowicz", "Bankier24", "S.A", 'Priority', "w:LsdException", "UnhideWhenUsed", "SemiHidden", "Stream", "Oproc",
                   "Survey", "View", "Szkwarek", "InfoMonitor", "Wrotniak", "Boczoń", "Limited", "Pay", "PRNews.pl", "Ensztein", "PRNews.pl", "naBankiera",
                   "RynekPierwotny.pl", "AŚ", "dataLayer.push", "eventAction", "eventCategory", "eventLabel", "eventNonInteraction", "eventValue", "promoView",
                   'Grid', "Profitable", "BB", "A2", 'Cenatorium', "e.createElement", "e.getElementById", "e.getElementsByTagName", "Home", 'InfogramEmbeds', "E3", "SII",
                   "A1", "3Q", "że", "się", "który", "raz", "być", "się", "że", "który", "swój", "TVN24", "red", "reda", "el", "and", "wielmożny", "poniedziałek",
                   "wtorek", "środa", "czwartek", "piątek", "sobota", "styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień",
                   "wrzesień", "październik", "listopad", "grudzień")

firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

stop_words_pl <- c(stop_words_pl, firstup(stop_words_pl))