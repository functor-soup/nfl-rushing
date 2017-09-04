# nfl-rushing

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
