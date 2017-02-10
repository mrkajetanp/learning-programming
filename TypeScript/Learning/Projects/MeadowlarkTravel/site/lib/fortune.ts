let fortuneCookies = [
  "Conquer your fears or they will conquer you.",
  "Rivers need springs.",
  "Do not fear what you don't know.",
  "You will have a pleasant surprise",
  "Whenever possible, keep it simple.",
];

export function getFortune () {
  return fortuneCookies[Math.floor(Math.random() * fortuneCookies.length)];
}
