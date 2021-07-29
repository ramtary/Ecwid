package com.ecwid.sqlp;


import javax.swing.*;
import java.awt.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SQL_Parser {
    private static final String[] operators = {" = "," > "," < "," >= "," <= "," <> "," != "," !< "," !> "};

    public static void main(String[] args) {
        String queryString = getQueryString();
        Query parsedQuery = parseThis(queryString);
        System.out.println(parsedQuery);
    }

    public static String getQueryString() {
        JFrame frame = new JFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        FileDialog fileDialog = new FileDialog(frame, "Выберите файл", FileDialog.LOAD);
        fileDialog.setDirectory(System.getProperty("user.dir") + "\\query");
        fileDialog.setFile("*.txt");
        fileDialog.setVisible(true);
        String fileName;
        try {
            fileName = fileDialog.getDirectory() + fileDialog.getFile();
            return new String(Files.readAllBytes(Paths.get(fileName)));
        } catch (Exception e) {
            System.exit(0);
        }
        return null;
    }


    public static Query parseThis(String queryString) {
        ArrayList<String> arrayOfQueryLines = new ArrayList<>(queryString.lines().toList());

        List<String> columns = new ArrayList<>();
        List<Query.Source> sources = new ArrayList<>();
        List<Query.Join> joins = new ArrayList<>();
        List<Query.WhereAndHavingClause> whereAndHavings = new ArrayList<>();
        List<String> groups = new ArrayList<>();
        List<Query.Sort> sorts = new ArrayList<>();
        Integer limit = 0, offset = 0;

        for (String line: arrayOfQueryLines) {
            if (line.toUpperCase().contains(KeyWords.SELECT.toString()))
                extSimple(columns, line, KeyWords.SELECT);

            if (line.toUpperCase().contains(KeyWords.FROM.toString()))
                extSources(sources, line);

            if (line.toUpperCase().contains(KeyWords.JOIN.toString()))
                extJoins(joins, line);

            if (line.toUpperCase().contains(KeyWords.WHERE.toString()) | line.toUpperCase().contains(KeyWords.HAVING.toString()))
                extWhereAndHavings(whereAndHavings, line);

            if (line.toUpperCase().contains(KeyWords.GROUP_BY.toString()))
                extSimple(groups, line, KeyWords.GROUP_BY);

            if (line.toUpperCase().contains(KeyWords.ORDER_BY.toString()))
                extSorts(sorts, line);

            if (line.toUpperCase().contains(KeyWords.LIMIT.toString()))
                limit = extTruncations(line, KeyWords.LIMIT);

            if (line.toUpperCase().contains(KeyWords.OFFSET.toString()))
                limit = extTruncations(line, KeyWords.OFFSET);
        }

        return new Query(columns, sources, joins, whereAndHavings, groups, sorts, limit, offset);
    }

    private static void extWhereAndHavings(List<Query.WhereAndHavingClause> whereAndHavings, String line) {
        String clearClauseString;
        boolean isHaving;

        List<Query.Clause> clauses = new ArrayList<>();

        if (line.toUpperCase().contains(KeyWords.WHERE.toString())) {
            clearClauseString = removeKeyWord(line.toUpperCase(), KeyWords.WHERE);
            isHaving = false;
        }
        else {
            clearClauseString = removeKeyWord(line.toUpperCase(), KeyWords.HAVING);
            isHaving = true;
        }
        String[] clausePartsAND = clearClauseString.split(KeyWords.AND.getWordInQuery());
        String[] clausePartsOR = clearClauseString.split(KeyWords.OR.getWordInQuery());

        if (clausePartsAND.length > 1) {
            extMultiClause(clauses, clausePartsAND, KeyWords.OR);
        }
        else {
            if (clausePartsOR.length > 1)
                extMultiClause(clauses, clausePartsOR, KeyWords.AND);
            else
                extSingleClause(clauses, clearClauseString);
        }
        whereAndHavings.add(new Query.WhereAndHavingClause(line.toUpperCase(), clauses, isHaving));
    }

    private static void extMultiClause(List<Query.Clause> clauses, String[] clauseParts, KeyWords keyWord) {
        String[] clausePartsDif;
        for (String andClause : clauseParts) {
            andClause = removeBrackets(andClause);
            clausePartsDif = andClause.split(keyWord.getWordInQuery());
            if (clausePartsDif.length > 1) {
                for (String orClause : clausePartsDif) {
                    extSingleClause(clauses, orClause);
                }
            } else {
                extSingleClause(clauses, andClause);
            }
        }
    }

    private static void extSingleClause(List<Query.Clause> clauses, String clause) {
        for (String operator : operators) {
            if (clause.contains(operator)) {
                String[] atr = clause.split(operator);
                clauses.add(new Query.Clause(atr[0], operator, atr[1]));
            }
        }
    }

    private static String removeKeyWord(String line, KeyWords keyWord) {
        return line.toUpperCase().replace(keyWord.getWordInQuery(), "").replace(";", "");
    }

    private static String removeBrackets(String line) {
        Pattern pattern = Pattern.compile("^\\((.*)\\)$");
        Matcher match = pattern.matcher(line);

        return match.find() ? match.group(1) : line;
    }

    private static void extJoins(List<Query.Join> joins, String line) {
        String joinString = line.toUpperCase();
        String joinType = joinString.substring(0, line.indexOf(" "));

        switch (joinType) {
            case "LEFT" -> joinString = removeKeyWord(removeKeyWord(joinString, KeyWords.LEFT), KeyWords.JOIN);
            case "RIGHT" -> joinString = removeKeyWord(removeKeyWord(joinString, KeyWords.RIGHT), KeyWords.JOIN);
            case "INNER" -> joinString = removeKeyWord(removeKeyWord(joinString, KeyWords.INNER), KeyWords.JOIN);
            default -> {
                joinString = removeKeyWord(joinString, KeyWords.JOIN);
                joinType = "";
            }
        }

        String[] joinParts = joinString.split(KeyWords.ON.getWordInQuery());
        Query.Join j = new Query.Join(joinType, joinParts[0], joinParts[1]);
        joins.add(j);
    }

    private static void extSources(List<Query.Source> sources, String line) {
        String sourceString = removeKeyWord(line.toUpperCase(), KeyWords.FROM);

        for (String source: sourceString.split(", ")) {
            Query.Source s;
            String[] sourceParts = source.split(" ");

            if (sourceParts.length > 1)
                s = new Query.Source(sourceParts[0], sourceParts[1]);
            else
                s = new Query.Source(source);
            sources.add(s);
        }
    }

    private static void extSorts(List<Query.Sort> sorts, String line) {
        String sortString = removeKeyWord(line.toUpperCase(), KeyWords.ORDER_BY);

        for (String sort: sortString.split(", ")) {
            Query.Sort s;
            String[] sortParts = sort.split(" ");

            if (sortParts.length > 1)
                s = new Query.Sort(sortParts[1], sortParts[0]);
            else
                s = new Query.Sort(sort);
            sorts.add(s);
        }
    }

    private static void extSimple(List<String> columns, String line, KeyWords keyWord) {
        String columnsString = removeKeyWord(line.toUpperCase(), keyWord);
        Collections.addAll(columns, columnsString.split(", "));
    }

    private static Integer extTruncations(String line, KeyWords keyWord) {
        String truncateString = removeKeyWord(line.toUpperCase(), keyWord);
        return Integer.getInteger(truncateString);
    }
}
