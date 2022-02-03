export const stringifySeconds = (seconds: number): string => {
  return `${("00" + Math.floor(seconds / 3600)).slice(-2)}:${("00" + Math.floor(seconds / 60)).slice(-2)}:${(
    "00" +
    (seconds % 60)
  ).slice(-2)}`;
}

export const stringifyPenalty = (penalty: number): string => {
  return penalty ? String(penalty) : "-";
}