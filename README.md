# chess221
<h2>"What is this?"</h2>
<p>
A Discord bot that you can play chess with using private messaging. It is capable of interpreting chess moves (as well as other commands) from text input, 
</p>
<h2>"Why would you make that?"</h2>
<p>
It was made for an obligatory assignment in INF221: Advanced Functional Programming.
</p>

<h2>"Oh really? How do I use it?"</h2>
<h4>Running the back-end:</h4>

<p>
Invite it to your server:
https://discord.com/oauth2/authorize?client_id=1209995597805916201&permissions=67584&scope=bot

Or you can try to join a test server that I created for it and message it from there:
https://discord.gg/r5fuKFXV

Then navigate to the folder with the stack file in it:
- cd chessbot-discord
- stack clean
- stack build
- stack run
</p>

<br>

<h4>Interacting with the front-end:</h4>
<p>
Send the bot a private message. Try "help" for a list of commands, or challange it by typing in the "e2 e4" opening. Other things you can do:
  - reset : restarts the board
  - show : print the board
  - castle king : castles kingside, if possible
  - castle queen : castles queenside, if possible
  - resign : quit the game
</p>

<h2>"Wow, did you make it all by yourself?"</h2>
<p>
Sure did. Gisle Illguth Kvamme, that's me and I wrote all the code here, except where noted.
</p>
