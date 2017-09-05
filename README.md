# nfl-rushing

# How to work this app

1. Hope you have docker running on your computer, if not please get a hold of it 
2. Clone this repo to your local machine
3. cd to the cloned directory
4. run `docker build -t chickenstock .` to build the image
5. run `docker run -it chickenstock -p 8000:8000` 
6. open `localhost:8000` on your browser


### Architecture
1. Fat Server, and Client is to be kept as thin as possible
2. Data is corrected and converted to a relatively nicer form before it is put to use


### Tech
1. Haskell and Scotty (Haskell Web Lib) for the Backend
2. Elm for the frontend
3. Docker for the deployment container


### API Main Response Structure ###

The Player Type is as follows:

```
  {    "Player" : String,
      ,"Team" : String, 
      ,"Pos" : String,
      ,"Att" : Float,
      ,"Att/G" : Float,
      ,"Yds" : Float,
      ,"Avg" : Float,
      ,"Yds/G" : Float, 
      ,"TD" : Float,
      ,"Lng" : Float,
      ,"1st" : Float,
      ,"1st%" : Float,
      ,"20+" : Float,
      ,"40+" : Float,
      ,"FUM" : Float
  }

```


In most cases the response structure will be as follows

```
{
   `payload` : [Player],
   `currentPage` : Int,
   `totalPages` : Int
}

```

### API Routes (Backend)

| Route | Http Verb | Descriptions | Return Type |
|-------|-----------|--------------|-------------|
| `/alive` | GET | heart beat route | text |
| `/players`    |    GET    | get all players | ApiResponse Type |
| `/players?limit={n}&page={i}`| GET | Get a apaginated version of players | ApiResponse Type |
| `/players?name={Something something}` | GET | Get a player by fuzzy matching on given name value | ApiResponse Type |
| `/players?sort={fieldName}&direction={asc :or: desc}` | GET | Get players sorted on field name and in a given direction (ascending or descending) | ApiResponse Type |
| `/players?csv=true`| GET | generally, one can append `csv=true` to the end of the search query and get the results in form of a csv | browser opens up a csv file |


### Improvements that should be made
1. elm code could be made a lot cleaner
2. haskell code could be made a lot cleaner
3. The frontend could be made a lot prettier
4. The user should be able to change the number of entries per page, though the rest functionality exists the feature is not present on the frontend
5. Tests
