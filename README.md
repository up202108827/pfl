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

Estado intermédio:

`board([
    [nonblock, nonblock, nonblock, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, nonblock],
    [nonblock, white, nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock],
    [empty, nonblock, black, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty],
    [nonblock, empty, nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock],
    [nonblock, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, nonblock, nonblock, nonblock]
]).`

Estado final:

`board([
    [nonblock, nonblock, nonblock, nonblock, white, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, wgoal, nonblock, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, black, nonblock, empty, nonblock, nonblock],
    [nonblock, empty, nonblock, empty, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, black, nonblock, empty, nonblock],
    [empty, nonblock, black, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty, nonblock, empty],
    [nonblock, empty, nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock],
    [nonblock, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, white, nonblock, empty, nonblock, nonblock],
    [nonblock, nonblock, nonblock, empty, nonblock, white, nonblock, white, nonblock, white, nonblock, white, nonblock, empty, nonblock, nonblock, nonblock],
    [nonblock, nonblock, nonblock, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, bgoal, nonblock, nonblock, nonblock, nonblock]
]).`

### Visualização do estado do jogo

- Menu

Após o uso do predicado play/0, é gerado um menu de boas vindas onde é perguntado o modo de jogo que pretende jogar. Tem como opções humano contra humano, humano contra computador e computador contra computador. 

Caso seja escolhido o modo de jogo humano contra humano, é perguntado qual é o primeiro jogador a jogar.

Caso seja escolhido o modo de jogo humano contra computador, é perguntado qual o nível de dificuldade desejado (fácil ou difícil) e quem joga primeiro, a pessoa ou o computador.

Caso seja escolhido o modo de jogo computador contra computador, é perguntado o nível de dificuldade do computador, isto é, o nível das decisões que serão tomadas.

Após serem escolhidas todas as opções, é gerado o tabuleiro inicial e é indicado quem é o jogador a jogar o primeiro turno. O programa não aceita opções além das indicadas, sendo necessário recomeçar no caso de ser dada uma opção diferente.

- display_game(GameState)

Tal como referido anteriormente, assim que escolhidas as opções, é gerado o estado de jogo inicial e indicado de que jogador é o turno. Para cada jogada pedimos ao utilizador as coordenadas da casa da peça que pretende mover e para onde a pretende mover.

### Validação e execução do movimento

Tentamos implementar a função is_valid_move para ver se um movimento é válido, seguindo as regras anteriormente explicadas. Como não conseguimos implementar, chamamos a move(GameState, Move, NewGameState) que cria um GameState novo com a peça alterada para a sua nova posição.

### Lista de movimentos válidos

Implementamos a função valid_moves que retornava uma lista com todos os movimentos possíveis de um jogador.

### Fim do jogo

O predicado game_over(GameState, Winner) analisa se um jogador conseguiu chegar à linha adversária, encerrando assim o jogo.

### Conclusões

Em conclusão, o facto de não termos conseguido implementar corretamente a função is_valid_move não nos permitiu ter o jogo a funcionar como esperado. Além de não validar as jogadas humanas, comprometia também as jogadas dos computadores pois não conseguimos criar a lista valid_moves, não sendo assim possível procurar jogadas para os mesmos.

## Bibliografia

https://boardgamegeek.com/boardgame/375056/differo
