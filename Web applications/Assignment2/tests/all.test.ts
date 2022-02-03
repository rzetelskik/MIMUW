import { driver, By, Builder, WebDriver, WebElement } from "mocha-webdriver";
import { expect } from "chai";
import * as Faker from "faker";

const host: string = `http://localhost:3000`;
const delayWaitTimeout: number = 200;
const elementWaitTimeout: number = 10000;

const logIn = async function (driver: WebDriver, username: string, password: string) {
  await driver.wait((await driver.find("button[id=login-button]")).isDisplayed(), elementWaitTimeout);
  await (await driver.find("button[id=login-button]")).doClick();
  await driver.wait((await driver.find("input[id=input-username]")).isDisplayed(), elementWaitTimeout);
  await (await driver.find("input[id=input-username]")).doSendKeys(username);
  await (await driver.find("input[id=input-password]")).doSendKeys(password);
  await (await driver.find("button[id=login-confirm]")).doClick();
};

const changePassword = async function (driver: WebDriver, oldPassword: string, newPassword: string) {
  await driver.wait((await driver.find("button[id=password-button")).isDisplayed(), elementWaitTimeout);
  await (await driver.find("button[id=password-button]")).doClick();
  await driver.wait((await driver.find("input[id=change-password]")).isDisplayed(), elementWaitTimeout);
  await (await driver.find("input[id=change-password]")).doSendKeys(oldPassword);
  await (await driver.find("input[id=change-password-new]")).doSendKeys(newPassword);
  await (await driver.find("input[id=change-password-new-repeat]")).doSendKeys(newPassword);
  await (await driver.find("button[id=change-password-confirm]")).doClick();
};

const delay = function (ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
};

const solveSecondQuiz = async function (driver: WebDriver) {
  await driver.wait((await driver.find("tbody[id=quiz-list]")).isDisplayed(), elementWaitTimeout);
  await (await driver.findElement(By.xpath(`//a[@href="` + "quiz_template.html?id=2" + `"]`))).doClick();

  const inputAnswer: WebElement = await driver.find("input[id=user-answer]");
  const nextButton: WebElement = await driver.find("button[id=button-next]");
  const finishButton: WebElement = await driver.find("button[id=button-finish]");
  const backToMenuButton: WebElement = await driver.find("button[id=back-to-menu");

  await delay(delayWaitTimeout);
  await inputAnswer.sendKeys(Faker.random.number());
  await nextButton.doClick();
  await delay(delayWaitTimeout);
  await inputAnswer.sendKeys(Faker.random.number());
  await driver.wait(finishButton.isEnabled(), elementWaitTimeout);
  await finishButton.doClick();
};

describe("Tests", function () {
  this.timeout(20000);

  this.beforeEach(async function () {
    await driver.get(host);
  });

  this.afterEach(async function () {
    await driver.manage().deleteAllCookies();
  });

  it("User is being logged out in every session after having changed the password", async function () {
    const secondDriver: WebDriver = await new Builder().forBrowser("firefox").build();
    await logIn(driver, "user1", "user1");

    await secondDriver.get(host);
    await logIn(secondDriver, "user1", "user1");
    await changePassword(secondDriver, "user1", "test");

    await driver.get(host);

    expect(await (await driver.find("button[id=login-button]")).isDisplayed()).to.be.equal(true);

    await secondDriver.quit();
  });

  it("Quiz table is being filled in, the chosen quiz downloads and is not accessible after it has been solved. Quiz gets added to stats.", async function () {
    await logIn(driver, "user2", "user2");

    await driver.wait((await driver.find("tbody[id=quiz-list]")).isDisplayed(), elementWaitTimeout);
    await (await driver.findElement(By.xpath(`//a[@href="` + "quiz_template.html?id=1" + `"]`))).doClick();

    const inputAnswer: WebElement = await driver.find("input[id=user-answer]");
    const nextButton: WebElement = await driver.find("button[id=button-next]");
    const finishButton: WebElement = await driver.find("button[id=button-finish]");
    const backToMenuButton: WebElement = await driver.find("button[id=back-to-menu");

    await delay(delayWaitTimeout);
    await inputAnswer.sendKeys(Faker.random.number());
    await nextButton.doClick();
    await delay(delayWaitTimeout);
    await inputAnswer.sendKeys(Faker.random.number());
    await nextButton.doClick();
    await delay(delayWaitTimeout);
    await inputAnswer.sendKeys(Faker.random.number());
    await nextButton.doClick();
    await delay(delayWaitTimeout);
    await inputAnswer.sendKeys(Faker.random.number());
    await driver.wait(finishButton.isEnabled(), elementWaitTimeout);
    await finishButton.doClick();
    await driver.wait((await driver.find("div[id=popup]")).isDisplayed(), elementWaitTimeout);
    await backToMenuButton.doClick();

    await delay(delayWaitTimeout);
    await driver.wait((await driver.find("tbody[id=stats-list]")).isDisplayed(), elementWaitTimeout);
    expect((await driver.findElements(By.xpath(`//a[@href="` + "quiz_template.html?id=1" + `"]`))).length).to.be.eql(0);
    expect((await driver.findElements(By.xpath(`//a[@href="` + "stats_template.html?id=1" + `"]`))).length).to.be.eql(1);
  });

  it("User gets an error when trying to send results of the same test twice", async function () {
    const secondDriver: WebDriver = await new Builder().forBrowser("firefox").build();

    await logIn(driver, "user2", "user2");

    await secondDriver.get(host);
    await logIn(secondDriver, "user2", "user2");

    await solveSecondQuiz(driver);

    try {
      await solveSecondQuiz(secondDriver);
      return false;
    } catch (err) {
      return true;
    } finally {
      await secondDriver.quit();
    }
  });
});
