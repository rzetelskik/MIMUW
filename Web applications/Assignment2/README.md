## BUILD

```npm run build``` builds the entire project  
```npm run build-client``` builds the client  
```npm run build-server``` builds the server

## LINTER

```npm run lint``` runs linter on the entire project  
```npm run lint-client``` runs linter on client  
```npm run lint-serwer``` runs linter on server

## DATABASE

```npm run createdb``` creates a database and populates it with demo data

## RUN

```npm run start```

## TESTING

```npm run start-test``` creates a test database and populates it with demo data, then runs a test server  
```npm run test``` runs the tests and deletes the demo database

To test the application run ```npm run start-test``` in one console. After it has started listening for requests run ```npm run test``` in another console.


## ADDING QUIZES

JSON quiz format:  

```
{
  id: number,
  introduction: string,
  questions: [
    {
        question: string,
        answer: number,
        penalty: number,
    },
  ]
}
```
