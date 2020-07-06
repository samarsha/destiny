"use strict";

const app = Elm.Main.init({
  node: document.getElementById("app")
});

// WebSocket

const host = location.host || "localhost:3000";
const socket = new WebSocket("ws://" + host);
socket.addEventListener("message", event => app.ports.receive.send(JSON.parse(event.data)));
app.ports.send.subscribe(data => socket.send(JSON.stringify(data)));

// Dragging

document.addEventListener("pointerdown", downEvent => {
  if (!ignoredDragElements.includes(downEvent.target.tagName.toLowerCase()) &&
      downEvent.target.closest("[data-draggable]") !== null) {
    prepareDrag(downEvent);
  }
});

const dragThreshold = 10;

const ignoredDragElements = ["button", "input", "textarea"];

const prepareDrag = downEvent => {
  const expectDragStart = moveEvent => {
    if (distance(mousePosition(downEvent))(mousePosition(moveEvent)) >= dragThreshold) {
      cancelDragStart();
      const draggablePosition = elementPosition(downEvent.target.closest("[data-draggable]"));
      const offset = subtract(mousePosition(moveEvent))(draggablePosition);
      app.ports.drag.send({ dragStart: offset });
      app.ports.drag.send({ dragMove: [mousePosition(moveEvent), draggables()] });
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

const subtract = p1 => p2 => ({ x: p1.x - p2.x, y: p1.y - p2.y });

const mousePosition = event => ({ x: event.clientX, y: event.clientY });

const elementPosition = element => {
  const rect = element.getBoundingClientRect();
  return { x: rect.left, y: rect.top };
};

const offset = event => ({
  x: event.offsetX + event.target.offsetLeft,
  y: event.offsetY + event.target.offsetTop
});

const distance = p1 => p2 => {
  const dx = p2.x - p1.x;
  const dy = p2.y - p1.y;
  return Math.sqrt(dx * dx + dy * dy);
};

const dragMove = moveEvent =>
  app.ports.drag.send({ dragMove: [mousePosition(moveEvent), draggables()] });

const dragEnd = _ => {
  document.removeEventListener("pointermove", dragMove);
  app.ports.drag.send({ dragEnd: [] });
};

const draggables = () => Array
  .from(document.querySelectorAll("[data-draggable]"))
  .map(draggable => {
    const id = draggable.dataset.draggable;
    const region = draggable.getBoundingClientRect();
    return { id, region };
  });
