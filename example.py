from pyswip import Prolog

prolog = Prolog()

#wczytaj zawartość głównego pliku prologa
prolog.consult("main.pl")
list(prolog.query("start(5,5)"))#rozpoczęcie gry

#przykład działania: d, s, d, a, d, s, d, s, d
key = input("Podaj akcje: ")
while(key != "q"):
    
    #start/2 - metoda, która resetuje wiedzę i tworzy nową planszę, jej dwa parametry to rozmiary planszy (wiersze, kolumny)
    if key == "r":
        list(prolog.query("start(5,5)"))
    
    #next_step/2 - główna metoda, która odpowiada za działanie silnika. W results zwracane są informacje co silnik chce zrobić. 
    #na ten moment zawsze zwracana jest albo lista pól do odkrycia, albo min (nigdy nie ma obu jednocześnie).
    #results[0]["Fields"] to pola które chce odkryć w danym kroku (obecnie nie rozróżnia czy chce odkryć to pole, bo nie ma nic lepszego do zrobienia, czy dlatego że wie że jest na pewno puste). Korzystając z listy tych pól należy zrobić "assert_fields", inaczej w każdym kroku silnik będzie mówił że chce odkryć to pole.
    #results[0]["Mines"] to pola na których według silnika są miny. Silnik automatycznie je u siebie zaznacza, więc nie ma potrzeby robić "assert_fields".
    elif key == "s":
        result = list(prolog.query("next_step(Fields,Mines)"))
        print(result)
        
    #draw/0 - funkcja pomocnicza, rysująca obecną interpretację wiedzy w silniku
    elif key == "d":
        list(prolog.query("draw"))
        
    #assert_fields/1 - pozwala dodać nową wiedzę do silnika. funkcja powinna być używana gdy next_step zwróci niepsutą tablicę z "Fields". 
    #jako argument przyjmuje listę, w skład której wchodzą naprzemiennie: tablica z kordami odkrytego pola i jego wartość (x = mina). 
    #poniżej przykład odkrycia 3 pól (pierwsza korda to wiersz, druga to kolumna). odkrycie pojedyńczego pola można zrealizować jako assert_fields([[X,Y],V]) lub można użyć assert_field([X,Y],V)
    elif key == "a":
        list(prolog.query("assert_fields([[1,1],1,[1,2],2,[1,3],1])"))
    
    key = input("Podaj akcje: ")