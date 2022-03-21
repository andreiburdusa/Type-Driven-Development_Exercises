module Chapter_12.Section_3.Exercises

-- Exercise 1
-- See Chapter_12.Section_3.ArithState

-- Exercise 2
-- See Chapter_12.Section_3.ArithState

-- Exercise 3
record Votes where
    constructor MkVotes
    upvotes : Integer
    downvotes : Integer

record Article where
    constructor MkArticle
    title : String
    url : String
    score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

getScore : Article -> Integer
getScore (MkArticle _ _ (MkVotes upvotes downvotes)) = upvotes - downvotes

getScore' : Article -> Integer
getScore' article = upvotes (score article) - downvotes (score article)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

-- Exercise 4
addUpvote : Article -> Article
addUpvote = {score->upvotes $= (+1)}

addDownvote : Article -> Article
addDownvote = {score->downvotes $= (+1)}
