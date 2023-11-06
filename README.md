# Differo

Grupo

Nuno Rodrigo Moreira Silva - 202108827

Tiago da Silva Azevedo - 202108840

## Instalação e Execução

Assim que instalado o SICStus Prolog, é necessário dar consult do ficheiro differo.pl, o que irá correr o resto dos ficheiros. A partir daí usar o predicado play (play.) permite aceder ao menu, em que apenas precisa de selecionar as opções com o modo de jogo pretendido.

## Descrição do jogo

Differo é um jogo para 2 jogadores, jogado num tabuleiro hexagonal com 13 peças pretas e 13 peças brancas, sendo que cada cor pertence a um dos jogadores.

Neste jogo, as peças podem ser movimentadas diagonal ou horizontalmente, tantas casas quantas peças o jogador tiver a mais que o seu adversário nesse sentido (diagonal ou horizontal), começando pelo jogador que está a jogar com as peças brancas. Caso o número seja igual ou inferior a 0, o jogador não pode mover nenhuma peça nesse sentido. As peças podem "saltar" peças da mesma cor ou do adversário mas não podem ir para nenhuma casa ocupada por outra peça.

Tem como objetivo chegar a uma das casas horizontais do lado contrário à que as suas peças se encontram, enquanto se preocupa em bloquear as peças adversárias. Por outro lado, caso o jogador não consiga mover nenhuma das suas peças, perde o jogo.

## Lógica do jogo

### Representação do estado interno do jogo

O tabuleiro é representado como uma lista de listas, onde cada elemento da lista interna representa uma casa do tabuleiro. Os elementos dessas listas internas podem ser peças do jogo ("white" para as peças brancas, "black" para as peças pretas), casas vazias representadas por "empty" ou células que não são usadas no jogo ("nonblock").

Estado inicial: 
`board([
    [nonblock, nonblock, nonblock, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, nonblock],
    [nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock],
    [empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty],
    [nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock],
    [nonblock, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, nonblock, nonblock, nonblock]
]).`

### Visualização do estado do jogo

### Validação e execução do movimento

### Lista de movimentos válidos

### Fim do jogo

### Avaliação do estado do jogo

### Jogadas do Computador

## Bibliografia

https://boardgamegeek.com/boardgame/375056/differo
