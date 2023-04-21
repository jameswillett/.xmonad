#!/usr/bin/env node
const { execSync } = require("child_process");

const e = (command) => execSync(command).toString().trim();

try {
  const status = e("playerctl status");
  if (status === "Stopped") {
    return;
  }
  const player = e("playerctl metadata -f '{{playerName}}'");

  let playerIcon = '';
  switch (player) {
    case "spotify":
      playerIcon = " "
      break;
    case "chromium":
      playerIcon = " "
  }

  let statusIcon = '';
  switch (status) {
    case "Playing":
      statusIcon = "▶️ "
      break;
    case "Paused":
      statusIcon = "⏸️ "
      break;
  }
  const string = e("playerctl metadata -f '{{artist}} - <fc=#ccc>{{title}}</fc> {{duration(position)}} ({{duration(mpris:length)}})'").replace(/^-\s/, "");
  console.log(playerIcon + statusIcon + string);
} catch {
}
