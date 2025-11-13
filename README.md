# Press Plotter

A web application that visualizes button press patterns over time.

## Features

- **Real-time visualization** of button press/release patterns
- **Time axis** with second markers for easy timeline reading
- **Grid background** for easy visual alignment
- **Horizontal scrolling** for long recordings
- **Duration display** showing total recording time
- **Recording indicator** to show active recording status
- **Clear button** to reset visualization without stopping
- **Ultra-high precision** tracking (millisecond-accurate timestamps for detecting extremely rapid presses)
- **Visual feedback** on button press with color change
- **Keyboard support** - Use spacebar to press/release
- **Statistics panel** - Real-time metrics (press count, averages, totals, fraction pressed)
- **Data export** - Export recordings as downloadable JSON file
- **Zoom controls** - Adjust timeline scale (10-800 pixels per second) for detailed or overview viewing
- **Auto-scroll** - Keeps timeline scrolled to show latest activity (toggleable, auto-disables on manual scroll)

## What it does

Press Plotter allows you to record and visualize when a button is pressed and released over time. The visualization displays a timeline with:
- **Blue rectangles**: Time periods when the button was pressed
- **Green rectangles**: Time periods when the button was released

## How to use

1. Click **"Start Recording"** to begin tracking
2. **Hold down** the "Hold Me to Record Press" button to record a press event (button turns blue when pressed)
3. **Release** the button to record a release event (button returns to green)
4. Repeat pressing and releasing to create a pattern
5. Watch the timeline grow with color-coded segments (auto-scrolls to show latest by default)
6. Use **Zoom +/−** buttons to adjust the timeline scale for more detail or overview
7. Toggle **"Auto-scroll"** to control whether timeline follows latest activity (scrolling left auto-disables it)
8. Click **"Clear"** to reset the visualization while keeping recording active
9. Click **"Export Data"** to download recording as JSON
10. Click **"Stop Recording"** to end the session

The timeline updates in real-time (approximately every 10ms) as you press and release the button, creating a visual representation of your press pattern with millisecond-accurate timing. All time measurements use system timestamps for precision timing regardless of display update rate. The timeline automatically scrolls horizontally for longer recordings. The default scale is 50 pixels per second, but you can zoom in (up to 800px/s for extreme detail) or zoom out (down to 10px/s for a compact overview) using the zoom controls.

## Development

### Prerequisites

- Elm 0.19.1

### Code Quality

The codebase follows best practices:
- **Type Safety**: Uses custom `ButtonState` type instead of primitive values
- **Named Constants**: All magic numbers extracted to `config` record
- **Helper Functions**: Reusable functions for calculations (no duplication)
- **Accessibility**: ARIA labels and semantic HTML throughout
- **Performance**: Optimized calculations with helper functions

### Building

```bash
elm make src/Main.elm --output=press-plotter.js
elm make src/Main.elm --output=press-plotter.min.js --optimize
```

### Running

Open `PressPlotter.html` in a web browser.

## Code Architecture

- **Model**: Stores events, recording state, and current button state
- **ButtonState**: Custom type (`Pressed | Released`) for type-safe state management
- **Event**: Records with startX position, length (in pixels), and button state
- **Config**: Centralized constants for all timing and layout values
- **Helper Functions**: Pure functions for time calculations and rendering
