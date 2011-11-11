% #################### Jeu de dames chinoises en Prolog

membre(X,[X|_]).
membre(X,[_|L]) :- membre(X,L).

% ================ Outils
odd(X) :- Y is X mod 2, Y==0.
even(X) :- not(odd(X)).

% ================ Modelisation du plateau
% Le plateau est limité à 2 triangles de hauteur 9 cases, qui correspond à un plateau
% pour 2 joueurs, modélisé comme ci-dessous
% x
% xx
% xxx
% xxxx
% xxxxx
% xxxxxx
% xxxxxxx
% xxxxxxxx
% xxxxxxxxx
% xxxxxxxx
% ...
% x


% ---------------- Un case appartient au plateau ?
case(X,Y) :- D is abs(Y-9), Y>0, Y=<17, X>0, X=<(9-D).

differents(X1,Y1,X2,Y2) :- X1\==X2; Y1\==Y2.

% ---------------- Les deux cases appartiennent au plateau ?
lien(X1,Y1,X2,Y2) :- (not(case(X1,Y1)); not(case(X2,Y2))), !, fail.
% ---------------- Lien horizontal
lien(X1,Y1,X2,Y2) :- Y1==Y2, Z is abs(X1-X2), Z == 1.
% ---------------- Lien vertical (diagonale gauche dans le vrai jeu)
lien(X1,Y1,X2,Y2) :- X1==X2, Z is abs(Y1-Y2), Z == 1.

% ---------------- Diagonale bas du plateau
lien(X1,Y1,X2,Y2) :- Y1 < 9, W is Y2-Y1, W == 1, Z is X2-X1, Z == 1.
lien(X1,Y1,X2,Y2) :- Y1 =< 9, W is Y2-Y1, W == -1, Z is X2-X1, Z == -1.

% ---------------- Diagonale haut du plateau
lien(X1,Y1,X2,Y2) :- Y1 >= 9, W is Y2-Y1, W == 1, Z is X1-X2, Z == 1.
lien(X1,Y1,X2,Y2) :- Y1 > 9, W is Y2-Y1, W == -1, Z is X1-X2, Z == -1.


% ================ Etat initial : placement des pions

:- dynamic ordi/2.
ordi(1,17).
ordi(1,16).
ordi(2,16).
ordi(1,15).
ordi(2,15).
ordi(3,15).
ordi(1,14).
ordi(2,14).
ordi(3,14).
ordi(4,14).

:- dynamic player/2.
player(1,1).
player(1,2).
player(2,2).
player(1,3).
player(2,3).
player(3,3).
player(1,4).
player(2,4).
player(3,4).
player(4,4).


% ================ Etats des cases : libre ou occupee
libre(X,Y) :- case(X,Y), not(player(X,Y)), not(ordi(X,Y)).
occupee(X,Y) :- case(X,Y), (player(X,Y);ordi(X,Y)).


% ================ Accessibilité des cases
% Les deplacements sont décrits par rapport a la modelisation du plateau
% Peut etre penser a enlever les ! lors de l integration de l ia

% ---------------- Deplacement avec saut sur la meme ligne a droite
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Diff is X2-X1, Diff==2, Xi is X1+1, Y1==Y2, occupee(Xi,Y1), !.
% ---------------- Deplacement avec saut sur la meme ligne a gauche
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Diff is X1-X2, Diff==2, Xi is X1-1, Y1==Y2, occupee(Xi,Y1), !.

% ---------------- Partie < 9 (il est possible darriver sur la ligne 9)
% ---------------- Deplacement oblique haut
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1<9, Y2=<9, DiffX is X2-X1, DiffY is Y2-Y1, (DiffX==2;DiffX==0), DiffY==2, Xi is X1+(DiffX/2), Yi is Y1+1, occupee(Xi,Yi), !.
% ---------------- Deplacement oblique bas
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1<9, Y2=<9, DiffX is X1-X2, DiffY is Y1-Y2, (DiffX==2;DiffX==0), DiffY==2, Xi is X1-(DiffX/2), Yi is Y1-1, occupee(Xi,Yi), !.


% ---------------- Partie > 9 (il est possible darriver sur la ligne 9)
% ---------------- Deplacement oblique haut
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1>9, Y2>=9, DiffX is X1-X2, DiffY is Y2-Y1, (DiffX==2;DiffX==0), DiffY==2, Xi is X1-(DiffX/2), Yi is Y1+1, occupee(Xi,Yi), !.
% ---------------- Deplacement oblique bas
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1>9, Y2>=9, DiffX is X2-X1, DiffY is Y1-Y2, (DiffX==2;DiffX==0), DiffY==2, Xi is X1+(DiffX/2), Yi is Y1-1, occupee(Xi,Yi), !.

% ---------------- Partie du milieu
% ---------------- Deplacement vers le haut (2 possibilites)
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1==8, Y2==10, DiffX is X2-X1, DiffX==1, Yi is Y1+1, occupee(X2,Yi).
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1==8, Y2==10, DiffX is X1-X2, DiffX==1, Yi is Y1+1, occupee(X1,Yi).
% ---------------- Deplacement vers le bas (2 possibilites)
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1==10, Y2==8, DiffX is X2-X1, DiffX==1, Yi is Y1-1, occupee(X2,Yi).
accessibleparsaut(X1,Y1,X2,Y2) :- differents(X1,Y1,X2,Y2), libre(X2,Y2), Y1==10, Y2==8, DiffX is X1-X2, DiffX==1, Yi is Y1-1, occupee(X1,Yi).


accessibleProche(X1,Y1,X2,Y2) :- lien(X1,Y1,X2,Y2),libre(X2,Y2).
accessible(X1,Y1,X2,Y2) :- lien(X1,Y1,X2,Y2),libre(X2,Y2),!.
accessible(X1,Y1,X2,Y2) :- accessibleparsaut(X1,Y1,X2,Y2),!.
% TODO : saut multiple
% ---------------- double saut
%accessible(X1,Y1,X2,Y2) :- numlist(1,9, DX),numlist(1,17, DY), membre(W, DX), membre(Z, DY), accessibleparsaut(X1,Y1,W,Z), accessibleparsaut(W,Z,X2,Y2).
%accessible(X1,Y1,X2,Y2) :- numlist(1,9, DX),numlist(1,17, DY),membre(W, DX), membre(Z, DY),differents(X1,Y1,X2,Y2),accessibleparsaut(X1,Y1,W,Z), accessibleparsaut(W,Z,X2,Y2).

% ================ Déplacements des pions
% ---------------- Player
deplacerplayer(X1,Y1,X2,Y2) :- player(X1,Y1), accessible(X1,Y1,X2,Y2), retract(player(X1,Y1)), assert(player(X2,Y2)),drawplateau(1),!.
% ---------------- Ordi
deplacerordi(X1,Y1,X2,Y2) :- ordi(X1,Y1),accessible(X1,Y1,X2,Y2), retract(ordi(X1,Y1)), assert(ordi(X2,Y2)),drawplateau(1),!.


% ================ Affichage

% Dessine o si la case existe, 1 pour le cpu, 2 pour le player
draw(A,B) :- ordi(A,B), write(1), !.
draw(A,B) :- player(A,B), write(2), !.
draw(A,B) :- case(A,B), write(.), !.
draw(_,_) :- write(-).

% ---------------- Dessine une ligne
drawline(Y,NB_CASES, I) :- (I=<(9-NB_CASES), write(' '), J is I+1, drawline(Y,NB_CASES, J));(I>9, writeln(' '), !);(I>(9-NB_CASES), J is I+1, K is (I-9+NB_CASES), draw(K,Y), write(' '), drawline(Y,NB_CASES, J))	.

% ---- Afichage collé à gauche (cf modélisation)
% drawline(Y,NB_CASES, I) :- (I>NB_CASES,writeln(' '), !);(J is I+1,draw(I,Y), drawline(Y,NB_CASES, J)).

% ----------------- Dessine le plateau en entier (dans le bon sens)
drawplateau(18).
drawplateau(Y) :- NB_CASES is (9 - abs(9-Y)), K is 18-Y, drawline(K,NB_CASES, 1), J is Y+1, drawplateau(J),!.


% ================ Mecanismes de Jeu
jouer(X1,Y1,X2,Y2) :- deplacerplayer(X1,Y1,X2,Y2).
victoirePlayer :- player(1,17),player(1,16),player(2,16),player(1,15),player(2,15),player(3,15),player(1,14),player(2,14),player(3,14),player(4,14).
victoireOrdi :- ordi(1,1),ordi(1,2),ordi(2,2),ordi(1,3),ordi(2,3),ordi(3,3),ordi(1,4),ordi(2,4),ordi(3,4),ordi(4,4).

% ================ Pour simplifier on interdit les sauts en arriere
% ---------------- Case arrière pour l IA
sautarriereIA(_,Y1,_,Y2) :- (Y2>Y1).
% ---------------- Case arrière pour le joueur
sautarrierePlayer(_,Y1,_,Y2) :- (Y1<Y2).



% ================ IA
% On ne peut pas reculer
% On utilise l algorithme min max alpha beta

% ---------------- Outils
% ---------------- Max et Min entre 2 elements
max(A,A,A).
max(A,B,A) :- A>B.
max(A,B,B) :- A<B. 

min(A,A,A).
min(A,B,A) :- A<B.
min(A,B,B) :- A>B.

% ---------------- Incrementation
incr(P,P2) :- P2 is P+1.
% ---------------- Decrementation
decr(P,P2) :- P2 is P-1.
% ---------------- Liste vide
vide([]).
% ---------------- Taille Liste
taille([], 0).
taille([_|Q], T) :- taille(Q,T2), T is T2+1.

% ---------------- Joueur suivant
nextPlayer(1,2).
nextPlayer(2,1).

% ---------------- Feuille atteinte ou fin de partie
testProfondeur(P,_,_) :- P == 0 ; victoirePlayer ; victoireOrdi.


% Generation de la position sous forme de liste de coordonnees de pions
ordi(Pos) :- findall([X,Y], ordi(X,Y), Pos).
player(Pos) :- findall([X,Y], player(X,Y), Pos).
ordiPlayer(Pos) :- ordi(P1),player(P2), append([P1],[P2],Pos).


% ---------------- Evaluation de la position
% L'evaluation de la situation est la somme des valeurs des pions
% On prend en compte 2 composantes :
% - la distance du pion à la position finale (coeff 4)
% - la mobilite pour eviter les pions isoles (coeff 1) : pas pris en compte finalement
% - la distance par rapport à l'axe central (coeff 2)


% ---- Mobilite d'un pion
% On privilégie les pions qui ont le moins de voisinnage direct, cad qu'ils ont bcp de cases accessibles par saut
mobilite(X,Y,Mob) :- findall(X2, (Z is X-1,W is X+1,numlist(Z,W, DX), Z2 is Y-1,W2 is Y+1,numlist(Z2,W2,DY), membre(X2, DX), membre(Y2, DY), accessibleProche(X,Y,X2,Y2)), M), taille(M,Mob).

% Distance par rapport à l'axe = penalise si le pion est trop excentre
distanceAxe(X,Y,Dist) :- Y>9,Nb is 18-Y, Milieu is Nb/2, Dist is abs(Milieu-X).
distanceAxe(X,Y,Dist) :- Y=<9, Milieu is Y/2, Dist is abs(Milieu-X).

% ---- Evaluation d'un pion
% afin de gagner en rapidite (pour l'instant), on desactive la mobilite

%evalPionOrdi(X,Y,Val) :- mobilite(X,Y,M), Dist is 17-Y-1, Val is (Dist*3)-M.
evalPionOrdi(X,Y,Val) :- distanceAxe(X,Y,Dist), Val is ((18-Y)*4-Dist*2).

evalPlateauOrdi([], 0).
evalPlateauOrdi([T|Q], Val) :- nth1(1,T,X), nth1(2,T,Y), evalPionOrdi(X,Y,Val2),evalPlateauOrdi(Q, Val1), Val is Val1+Val2.
evaluationOrdi(Val) :- ordi(Pos), evalPlateauOrdi(Pos, Val).

%evalPionPlayer(X,Y,Val) :- mobilite(X,Y,M), Dist is Y+1, Val is (Dist*3)-M.
evalPionPlayer(X,Y,Val) :- distanceAxe(X,Y,Dist), Val is ((Y)*4-Dist*2).

evalPlateauPlayer([], 0).
evalPlateauPlayer([T|Q], Val) :- nth1(1,T,X), nth1(2,T,Y), evalPionPlayer(X,Y,Val2),evalPlateauPlayer(Q, Val1), Val is Val1+Val2.
evaluationPlayer(Val) :- player(Pos), evalPlateauPlayer(Pos, Val).


% -------------- Heuristiques
% ------ Avancement (distance à l'arrivée)

heur(Pos,_,Val,IA) :- 
	IA==1,	
	nth1(1,Pos,PionsOrdi), nth1(2,Pos,PionsPlayer), 
	heurAvancementOrdi(PionsOrdi,ValOrdi), 
	heurAvancementPlayer(PionsPlayer,ValPlayer),
	Val is ValOrdi-ValPlayer.

heur(Pos,_,Val,IA) :- 
	IA==2,	
	nth1(1,Pos,PionsOrdi), nth1(2,Pos,PionsPlayer), 
	heurAvancementOrdiBis(PionsOrdi,ValOrdi), 
	heurAvancementPlayerBis(PionsPlayer,ValPlayer),
	Val is ValPlayer-ValOrdi.


heurAPionOrdi(X,Y,Val) :- distanceAxe(X,Y,Dist), Val is ((18-Y)*4-Dist*2).
heurAvancementOrdi([],0).
heurAvancementOrdi([T|Q],Val) :- 
	nth1(1,T,X), nth1(2,T,Y), heurAPionOrdi(X,Y,Val2), 
	heurAvancementOrdi(Q,Val1), Val is Val1+Val2.

heurAPionPlayer(_,Y,Val) :- Val is (Y*4).
heurAvancementPlayer([],0).
heurAvancementPlayer([T|Q],Val) :- 
	nth1(1,T,X), nth1(2,T,Y), heurAPionPlayer(X,Y,Val2), 
	heurAvancementPlayer(Q,Val1), Val is Val1+Val2.


% Pour faire affronter 2 IA
heurAPionOrdiBis(X,Y,Val) :- distanceAxe(X,Y,_), Val is ((18-Y)*4).
heurAvancementOrdiBis([],0).
heurAvancementOrdiBis([T|Q],Val) :- nth1(1,T,X), nth1(2,T,Y), heurAPionOrdiBis(X,Y,Val2), heurAvancementOrdiBis(Q,Val1), Val is Val1+Val2.

heurAPionPlayerBis(X,Y,Val) :- distanceAxe(X,Y,Dist), Val is (Y*4)-Dist*2.
heurAvancementPlayerBis([],0).
heurAvancementPlayerBis([T|Q],Val) :- nth1(1,T,X), nth1(2,T,Y), heurAPionPlayerBis(X,Y,Val2), heurAvancementPlayerBis(Q,Val1), Val is Val1+Val2.



% ================== Positions accessibles
% Renvoie la liste des cases accessibles depuis la case de coordonnees X,Y
casesAccessiblesOrdi(X,Y,M) :- findall([X2,Y2], (numlist(1,9, DX), numlist(1,Y, DY), membre(X2, DX), membre(Y2, DY), accessible(X,Y,X2,Y2)), M).
casesAccessiblesPlayer(X,Y,M) :- findall([X2,Y2], (numlist(1,9, DX), numlist(Y,17, DY), membre(X2, DX), membre(Y2, DY), accessible(X,Y,X2,Y2)), M).
casesAccessibles(X,Y,M,Joueur,IA) :- IA==1,((Joueur==1, casesAccessiblesOrdi(X,Y,M),!);(Joueur==2, casesAccessiblesPlayer(X,Y,M),!)).
casesAccessibles(X,Y,M,Joueur,IA) :- IA==2,((Joueur==2, casesAccessiblesOrdi(X,Y,M),!);(Joueur==1, casesAccessiblesPlayer(X,Y,M),!)).

% ------- Jouer le Pion
simulerCoupOrdi(Pos,X1,Y1,X2,Y2,Out) :- subtract(Pos,[[X1,Y1]], G),append([[X2,Y2]],G,Out).

distribuerPlayer([],_,[]).
distribuerPlayer([T|Q],Elem,List) :- distribuerPlayer(Q,Elem,L2),append([T],[Elem],L1),append([L1],L2,List).

distribuerOrdi([],_,[]).
distribuerOrdi([T|Q],Elem,List) :- distribuerOrdi(Q,Elem,L2),append([Elem],[T],L1),append([L1],L2,List).

% ------- Successeurs de la position courante = tous les prochains coups possibles
succL([],_,[]).
succL([Elem|QL],G,Res) :- 
	succL(QL,G,Res1),
	append([Elem],G,Res2),
	append(Res1,[Res2],Res).

succCalc([],_,[],_,_).
succCalc([T|Q],Pos,PosList,Joueur,IA) :- 
	nth1(1,T,Xi),nth1(2,T,Yi),
	casesAccessibles(Xi,Yi,L,Joueur,IA),
	subtract(Pos,[[Xi,Yi]],G),
	succL(L,G,PosList1),
	succCalc(Q,Pos,PosList2,Joueur,IA),
	append(PosList1,PosList2,PosList).


% IA est utile pour faire jouer deux IA entre elles
succ([Ordi,Player],Joueur,PosList,IA) :- 
	IA==1,Joueur==1,
	succCalc(Ordi,Ordi,Temp,Joueur,IA), 
	distribuerPlayer(Temp,Player,PosList).%,writeln(PosList).

succ([Ordi,_],Joueur,PosList,IA) :- 
	IA==1,Joueur==2,
	succCalc(Ordi,Ordi,Temp,Joueur,IA), 
	distribuerOrdi(Temp,Ordi,PosList).%,writeln(PosList).

succ([Ordi,Player],Joueur,PosList,IA) :- 
	IA==2,Joueur==1,
	succCalc(Player,Player,Temp,Joueur,IA),
	distribuerOrdi(Temp,Ordi,PosList).%,writeln(PosList).

succ([_,Player],Joueur,PosList,IA) :- 
	IA==2,Joueur==2,
	succCalc(Player,Player,Temp,Joueur,IA),
	distribuerPlayer(Temp,Player,PosList).%,writeln(PosList).


% ---------------- Minimax Alpha Beta

% MAX = 1, MIN = 2
secondJoueur(1,2).
secondJoueur(2,1).

copie(Position,Valeur,Position,Valeur).

% Prédicats pour se souvenir des valeurs des feuilles et les faire remonter
recordMax(X,X,E,_,E,X).
recordMax(X,Y,E,_,E,X) :- X>=Y.
recordMax(X,Y,_,MeilleurL,MeilleurL,Y) :- X=<Y.

recordMin(X,X,E,_,E,X).
recordMin(X,Y,E,_,E,X) :- X=<Y.
recordMin(X,Y,_,MeilleurL,MeilleurL,Y) :- X>=Y.


% Minimax alpha beta: appel
minimaxab(Position,Joueur,Profondeur,Meilleur,Valeur,IA) :-
	succ(Position,Joueur,X,IA), % Simulation du coup, X = liste des positions apres avoir joué
	secondJoueur(Joueur,AutreJoueur), % Changement de joueur
	decr(Profondeur,P2), % Décrémentation de la profondeur
	alphabeta(X,AutreJoueur,P2,-999,999,Meilleur,Valeur,IA). % Alpha et Beta sont iniatilisés à -999 et 999


alphabeta([],_,_,_,_,_,_,_).

alphabeta([E|L],1,P,A,B,Meilleur,V,IA) :-
        testProfondeur(P,E,1), % Feuille atteinte ou fin de partie
        heur(E,1,ValE,IA), % Evaluation de la position dans ValE
        ((A=<ValE, % Evaluation meilleure
          alphabeta(L,1,P,A,B,MeilleurL,ValL,IA),
          recordMin(ValE,ValL,E,MeilleurL,Meilleur,V))
         ;
         copie(E,ValE,Meilleur,V)). % Memorisation du meilleur

alphabeta([E|L],2,P,A,B,Meilleur,V,IA) :-
        testProfondeur(P,E,2),
        heur(E,2,ValE,IA),
        ((B>=ValE,
          alphabeta(L,2,P,A,B,MeilleurL,ValL,IA),
          recordMax(ValE,ValL,E,MeilleurL,Meilleur,V))
         ;
         copie(E,ValE,Meilleur,V)).

alphabeta([E|L],1,P,A,B,Meilleur,V,IA) :-
        succ(E,1,X,IA),
        not(vide(X)),
        P\=0,
        decr(P,P2),
        alphabeta(X,2,P2,A,B,MeilleurX,ValX,IA),
        ((ValX>=A,
          min(ValX,B,Bbis),
          alphabeta(L,1,P,A,Bbis,MeilleurL,ValL,IA),
          recordMin(ValX,ValL,E,MeilleurL,Meilleur,V))
         ;
         copie(MeilleurX,ValX,Meilleur,V)).
    
alphabeta([E|L],2,P,A,B,Meilleur,V,IA) :-
        succ(E,2,X,IA),
        not(vide(X)),
        P\=0,
        decr(P,P2),
        alphabeta(X,1,P2,A,B,MeilleurX,ValX,IA),
        ((ValX=<B,
          max(ValX,A,Abis),
          alphabeta(L,2,P,Abis,B,MeilleurL,ValL,IA),
          recordMax(ValX,ValL,E,MeilleurL,Meilleur,V))
         ;
         copie(MeilleurX,ValX,Meilleur,V)).


% Appel pour faire jouer l'ordi

jouerOrdi(IA) :- ordiPlayer(Pos), minimaxab(Pos,1,3,Meilleur,_,IA),!, retractall(ordi(_,_)),nth1(1,Meilleur,Best),
	forall(
		member(Elem,Best),
		(
			nth1(1,Elem,X1), nth1(2,Elem,Y1), assert(ordi(X1,Y1))
		)
	),
	drawplateau(1),!.


jouerPlayer(IA) :- ordiPlayer(Pos), minimaxab(Pos,1,2,Meilleur,_,IA),!, retractall(player(_,_)),nth1(2,Meilleur,Best),
	forall(
		member(Elem,Best),
		(
			nth1(1,Elem,X1), nth1(2,Elem,Y1), assert(player(X1,Y1))
		)
	),
	drawplateau(1),!.
	

% pour regarder un match entre 2 IA
go :- repeat, jouerOrdi(1),jouerPlayer(2).


