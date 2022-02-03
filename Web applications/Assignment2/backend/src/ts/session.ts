import { asyncDbRun } from "./database_utils";
import * as sqlite from "sqlite3";

export const destroySession = async (session: Express.Session): Promise<void> => {
  session.destroy((err) => {
    if (err) throw err;
  });
};

export const destroyAllSessions = async (db: sqlite.Database, user: string): Promise<void> => {
  await asyncDbRun(db, `DELETE FROM sessions WHERE sess LIKE '%"authorised":"' || ? || '"%'`, [user]);
};
