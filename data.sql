SELECT 
  *
FROM
  `retrosheet`.`gamelogs` 
WHERE MID(GameID, 4, 4) BETWEEN 1995 AND 2010;

SELECT 
  CONCAT(m.nameFirst, " ", m.nameLast) AS NAME, b.*
FROM
  `lahmandb`.`Batting` b 
JOIN lahmandb.`Master` m ON b.`playerID` = m.`playerID`
JOIN lahmandb.`Appearances` a ON b.`playerID` = a.`playerID` AND b.`yearID` = a.`yearID` AND b.`teamID` = a.`teamID`
WHERE b.yearID BETWEEN 1995 AND 2010
AND a.`G_p` = 0;
