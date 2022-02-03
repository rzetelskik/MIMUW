export interface Error {
  status?: number;
  syscall?: string;
  code?: string;
  message?: string;
}

export const createStatusError = (status: number, message: string) => {
  const err: Error = new Error(message);
  err.status = status;
  return err;
};
