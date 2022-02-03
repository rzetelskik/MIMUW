import * as sqlite from "sqlite3";
import bcrypt from "bcryptjs";

export const dbName: string = process.env.DBNAME;

export const databaseConnect = async (option: number): Promise<sqlite.Database> => {
  return new Promise((resolve, reject) => {
    const db: sqlite.Database = new sqlite.Database(dbName, option, (err) => {
      if (err) {
        reject(err);
        return;
      }
      resolve(db);
    });
  });
};

export const databaseDisconnect = async (db: sqlite.Database): Promise<void> => {
  return new Promise((resolve, reject) => {
    db.close((err: any) => {
      if (err) {
        reject(err);
        return;
      }
      resolve();
    });
  });
};

export const asyncDbRun = async (db: sqlite.Database, sql: string, params?: any[]): Promise<number> => {
  return new Promise((resolve, reject) => {
    db.run(sql, params, function (err) {
      if (err) {
        reject(err);
        return;
      }
      resolve(this.lastID);
    });
  });
};

export const asyncDbGet = async (db: sqlite.Database, sql: string, params?: any[]): Promise<any> => {
  return new Promise((resolve, reject) => {
    db.get(sql, params, (err, row) => {
      if (err) {
        reject(err);
        return;
      }
      resolve(row);
    });
  });
};

export const asyncDbAll = async (db: sqlite.Database, sql: string, params?: any[]): Promise<any[]> => {
  return new Promise((resolve, reject) => {
    db.all(sql, params, (err, rows) => {
      if (err) {
        reject(err);
        return;
      }
      resolve(rows);
    });
  });
};

export const asyncCreateUser = async (db: sqlite.Database, username: string, password: string): Promise<void> => {
  const hashedPassword: string = await bcrypt.hash(password, await bcrypt.genSalt(parseInt(username, 10)));
  await asyncDbRun(db, `INSERT OR REPLACE INTO users (username, password) VALUES (?, ?)`, [username, hashedPassword]);
};

export const asyncCheckPassword = async (db: sqlite.Database, user: string, password: string): Promise<boolean> => {
  const row: any = await asyncDbGet(db, `SELECT password FROM users WHERE username = ?`, [user]);
  if (!row) throw Error("User does not exist.");

  return bcrypt.compare(password, row.password);
};

export const asyncChangePassword = async (db: sqlite.Database, username: string, password: string): Promise<void> => {
  if (!password.trim()) throw new Error("Password can't be empty.");

  const hashedPassword: string = await bcrypt.hash(password, await bcrypt.genSalt(parseInt(username, 10)));
  await asyncDbRun(db, `UPDATE users SET password = ? WHERE username = ?`, [hashedPassword, username]);
};
