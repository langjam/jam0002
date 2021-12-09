import * as wasm from 'agents';

const CELL_SIZE = 5;
const GRID_COLOR = "#CCCCCC";

// Construct the universe, and get its width and height.
const width = wasm.board_width();
const height = wasm.board_height();

const RED_MASK = 0xff << 16;
const GREEN_MASK = 0xff << 8;
const BLUE_MASK = 0xff;

// Give the canvas room for all of our cells and a 1px border
// around each of them.
const canvas = document.getElementById('canvas');
canvas.height = (CELL_SIZE + 1) * height + 1;
canvas.width = (CELL_SIZE + 1) * width + 1;

const ctx = canvas.getContext('2d');

function drawGrid() {
  ctx.beginPath();
  ctx.strokeStyle = GRID_COLOR;

  // Vertical lines.
  for (let i = 0; i <= width; i++) {
    let x = i * (CELL_SIZE + 1) + 1;
    let y = (CELL_SIZE + 1) * height + 1;
    ctx.moveTo(x, 0);
    ctx.lineTo(x, y);
  }

  // Horizontal lines.
  for (let j = 0; j <= height; j++) {
    let x = (CELL_SIZE + 1) * width + 1;
    let y = j * (CELL_SIZE + 1) + 1;
    ctx.moveTo(0, y);
    ctx.lineTo(x, y);
  }

  ctx.stroke();

  let colors = wasm.tick();
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      let i = y * height + x;
      let color = colors[i];
      ctx.fillStyle = 'rgb(' + (color & RED_MASK).toString() + ',' + (color & GREEN_MASK).toString() + ',' + (color & BLUE_MASK).toString() + ')';
      ctx.fillRect((CELL_SIZE + 1) * x + 2, (CELL_SIZE + 1) * y + 2, CELL_SIZE - 1, CELL_SIZE - 1);
    }
  }
}

let fps = 10;

function loop() {
  drawGrid();

  setTimeout(() => {
    requestAnimationFrame(loop);
  }, 1000.0 / fps);
}

document.getElementById('runcode').addEventListener('click', () => {
  wasm.run(editor.getValue());
});

loop();

document.getElementById('editor').innerHTML = `# A program is composed of one or more independent agents.
# Agents move around the board in the direction they are oriented in on cycle
# of ticks, leaving behind a colored wake in their path.
#
# Agents are defined by by using the "agent" keyword followed by a name and
# semicolon.
agent red_agent:
    # Everything in a agent block is a statement.
    #
    # Statements can be labels or commands.
    # 
    # One such command is "set", used for defining variables.
    # Some variables are special and impact an agents placement or effect on
    # the board.
    # These include color, x and y for cell color or coordinates.
    set color = 0xFF0000
    set x = 40
    set y = 40
    # Other variables can be general purpose.
    set acc = 1
    set radius = 2
    # Commands control either agent behavior or control flow.
    #
    # face changes orientation of an agent and takes a cardinal direction, like
    # S, E or NW.
    face NW
    # Labels allow defining locations in code for a program to jump to
    loop:
        # Move takes a positive 32 bit integer and controls the number of steps
        # to move in a tick in this case our agent will move one space per tick
        # in the direction they are facing.
        move 1
        # Agent scripts support branching through the "jump to" command.
        # These commands look like:
        # "jump to <label> if <expression> is <expresion>".
        jump to spin if acc is radius
        # Variables also support basic arithmetic expressions.
        set acc = acc + 1
        # The goto command allows for the basic jumping to a label without a
        # condition.
        goto loop
    spin:
        # The turn command allows spinning an agent based on their current 
        # orientation. With a positive number being a clockwise rotation and a
        # negative being counter-clockwise.
        turn 1
        set acc = 1
        set radius = radius + 1
        goto loop
agent blue_agent:
    set color = 0xFF
    set x = 40
    set y = 40
    face NE
    loop:
        move 2
        goto loop
`

let editor = ace.edit('editor');
editor.setTheme("ace/theme/monokai");
