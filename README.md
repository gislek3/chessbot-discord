TODO:<br>
221:<br>
- Use the actual State monad in GameHandler / Game instead of your homebrew
- Use the Reader monad in order to cut down on "game" and "board" parameters?
<br>
General:<br>
- Clean up exports of modules, try to minimize, move functions as needed, split modules as needed
- Implement takeback command
- Implement en passant
- Implement flipping the display
- Implement more custom starts (endgame practice) (robot doesn't respond / offline multiplayer lmao) (simulation, which is that both colors are controlled by robot) maybe instead of botColor you should do an enum like whiteControlled Player and blackControlled Player, where Player = Human | Computer
<br>
Make the robot:<br>
- Make a function that returns a random legal move for a given color
- Make it so that the non-player 

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

- cd chessbot-discord
- stack clean
- stack build
- stack run
</p>

<br>

<h4>Interacting with the front-end:</h4>
<p>

- Invite the bot to a server (probably your own).
- Interact with the bot by sending it a private message (it will not respond to messages in public chat, this is to avoid spam).
- Type "help" for a list of commands.
- Try to beat the world's dumbest chess computer.
</p>

<h2>"Wow, did you make it all by yourself?"</h2>
<p>
Sure did. Gisle Illguth Kvamme, that's me and I wrote all the code here, except where noted.
</p>