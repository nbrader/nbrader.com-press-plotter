# Press Plotter

A web application that visualizes button press patterns over time.

## Features

- **Real-time visualization** of button press/release patterns
- **Time axis** with second markers for easy timeline reading
- **Horizontal scrolling** for long recordings
- **Duration display** showing total recording time
- **Recording indicator** to show active recording status
- **Clear button** to reset visualization without stopping
- **High resolution** tracking (100ms intervals)
- **Visual feedback** on button press with color change

## What it does

Press Plotter allows you to record and visualize when a button is pressed and released over time. The visualization displays a timeline with:
- **Blue rectangles**: Time periods when the button was pressed
- **Green rectangles**: Time periods when the button was released

## How to use

1. Click **"Start Recording"** to begin tracking
2. **Hold down** the "Hold Me to Record Press" button to record a press event (button turns blue when pressed)
3. **Release** the button to record a release event (button returns to green)
4. Repeat pressing and releasing to create a pattern
5. Watch the timeline grow with color-coded segments
6. Click **"Clear"** to reset the visualization while keeping recording active
7. Click **"Stop Recording"** to end the session

The timeline updates in real-time every 100ms as you press and release the button, creating a visual representation of your press pattern. The timeline automatically scrolls horizontally for longer recordings.

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
