# igo.el
Emacs mode to play the game of go/baduk/weichi

Currently has "view mode" and "play mode". In view mode, you can navigate through a loaded SGF file using the arrow keys. Play mode let's you add stones to the board (b/w in alternation). It is still farily primitive but is functionnal (mostly). 

To do:

- Refactor the overlay code to abstract the position of the board in the buffer (now takes for granted the board is on top, for instance).
- Better support <s-left> and <s-right>. It doesn't always bring you 10 moves backward/forward if there are branches in between.
- Better support <up> to go up the parent branch if you branch index is 0
- Edit mode: new mode to edit game information.
 - SGF export
