# Press Plotter

A web application that visualizes button press patterns over time.

## What it does

Press Plotter allows you to record and visualize when a button is pressed and released over time. The visualization displays a timeline with:
- **Blue rectangles**: Time periods when the button was pressed
- **Green rectangles**: Time periods when the button was released

## How to use

1. Click **"Start Recording"** to begin tracking
2. **Hold down** the "Hold Me to Record Press" button to record a press event
3. **Release** the button to record a release event
4. Repeat pressing and releasing to create a pattern
5. Click **"Stop Recording"** to end the session

The timeline updates in real-time as you press and release the button, creating a visual representation of your press pattern.

## Development

### Prerequisites

- Elm 0.19.1

### Building

```bash
elm make src/Main.elm --output=press-plotter.js
elm make src/Main.elm --output=press-plotter.min.js --optimize
```

### Running

Open `PressPlotter.html` in a web browser.
