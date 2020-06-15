"use strict";

const app = Elm.Main.init({
  node: document.getElementById("main")
});

// WebSocket

const host = location.host || "localhost:3000";
const socket = new WebSocket("ws://" + host);
socket.addEventListener("message", event => app.ports.receive.send(JSON.parse(event.data)));
app.ports.send.subscribe(data => socket.send(JSON.stringify(data)));

// Dragging

document.addEventListener("pointerdown", downEvent => {
  if (!ignoredDragElements.includes(downEvent.target.tagName.toLowerCase())) {
    prepareDrag(downEvent);
  }
});

const dragThreshold = 10;

const ignoredDragElements = ["button", "input", "textarea"];

const prepareDrag = downEvent => {
  const expectDragStart = moveEvent => {
    if (distance(position(downEvent))(position(moveEvent)) >= dragThreshold) {
      cancelDragStart();
      app.ports.drag.send({ position: position(moveEvent), draggables: draggables() });
      document.addEventListener("pointermove", dragMove);
      document.addEventListener("pointerup", dragEnd);
    }
  };

  const cancelDragStart = _ => {
    document.removeEventListener("pointermove", expectDragStart);
    document.removeEventListener("pointerup", cancelDragStart);
  };

  downEvent.preventDefault();
  document.addEventListener("pointermove", expectDragStart);
  document.addEventListener("pointerup", cancelDragStart);
};

const position = event => ({ x: event.clientX, y: event.clientY });

const distance = p1 => p2 => {
  const dx = p2.x - p1.x;
  const dy = p2.y - p1.y;
  return Math.sqrt(dx * dx + dy * dy);
};

const dragMove = moveEvent =>
  app.ports.drag.send({ position: position(moveEvent), draggables: draggables() });

const dragEnd = _ => document.removeEventListener("pointermove", dragMove);

const draggables = () => Array
  .from(document.querySelectorAll("[data-draggable]"))
  .map(draggable => {
    const id = draggable.dataset.draggable;
    const region = draggable.getBoundingClientRect();
    return { id, region };
  });
