package com.ecwid.sqlp;


import javax.swing.*;
import java.awt.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SQL_Parser {
    private static final String[] operators = {"=",">","<",">=","<=","<>","!=","!<","!>"};

    public static void main(String[] args) {
        final String ANSI_RESET = "\u001B[0m";
        final String ANSI_BLUE = "\u001B[34m";
        final String ANSI_GREEN = "\u001B[32m";

        String queryString = getQueryString();
        Query parsedQuery = parseThis(queryString);
        System.out.println(ANSI_GREEN + "Был запрос:" + ANSI_RESET);
        System.out.println(queryString);
        System.out.println();
        System.out.println(ANSI_GREEN + "Получился экземпляр класса Query:" + ANSI_RESET);
        System.out.println(ANSI_BLUE + parsedQuery + ANSI_RESET);
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
        List<Query.TwinExpr> sources = new ArrayList<>();
        List<Query.Join> joins = new ArrayList<>();
        List<Query.WhereAndHavingClause> whereAndHaving = new ArrayList<>();
        List<String> groups = new ArrayList<>();
        List<Query.TwinExpr> sorts = new ArrayList<>();
        int limit = 0, offset = 0;
        List<Query> subQuery = new ArrayList<>();

        for (String line: arrayOfQueryLines) {
            line = line.toUpperCase();

            if (line.contains(KeyWords.SELECT.toString()) & line.indexOf(KeyWords.SELECT.toString()) == 0)
                extSimple(columns, line, KeyWords.SELECT, subQuery);

            if (line.contains(KeyWords.FROM.toString()) & line.indexOf(KeyWords.FROM.toString()) == 0)
                extTwinExpr(sources, line, KeyWords.FROM);

            if (line.contains(KeyWords.JOIN.toString()))
                extJoins(joins, line);

            if ((line.contains(KeyWords.WHERE.toString()) & line.indexOf(KeyWords.WHERE.toString()) == 0)
                    | (line.contains(KeyWords.HAVING.toString()) & line.indexOf(KeyWords.HAVING.toString()) == 0))
                extWhereAndHaving(whereAndHaving, line, subQuery);

            if (line.contains(KeyWords.GROUP_BY.toString()) & line.indexOf(KeyWords.GROUP_BY.toString()) == 0)
                extSimple(groups, line, KeyWords.GROUP_BY, subQuery);

            if (line.contains(KeyWords.ORDER_BY.toString()))
                extTwinExpr(sorts, line, KeyWords.ORDER_BY);

            if (line.contains(KeyWords.LIMIT.toString()))
                limit = extTruncations(line, KeyWords.LIMIT);

            if (line.contains(KeyWords.OFFSET.toString()))
                offset = extTruncations(line, KeyWords.OFFSET);
        }

        return new Query(columns, sources, joins, whereAndHaving, groups, sorts, limit, offset, subQuery);
    }

    private static void extWhereAndHaving(List<Query.WhereAndHavingClause> whereAndHaving, String line, List<Query> subQuery) {
        String clearClauseString = "";
        boolean isHaving = false;

        List<Query.Clause> clauses = new ArrayList<>();

        if (line.contains(KeyWords.WHERE.toString()) & line.indexOf(KeyWords.WHERE.toString()) == 0) {
            clearClauseString = removeKeyWord(line, KeyWords.WHERE);
        }
        else {
            if (line.indexOf(KeyWords.HAVING.toString()) == 0) {
                clearClauseString = removeKeyWord(line, KeyWords.HAVING);
                isHaving = true;
            }
        }
        String[] clausePartsAND = clearClauseString.split(KeyWords.AND.getWordInQuery());
        String[] clausePartsOR = clearClauseString.split(KeyWords.OR.getWordInQuery());

        if (clausePartsAND.length > 1) {
            extMultiClause(clauses, clausePartsAND, KeyWords.OR, subQuery);
        }
        else {
            if (clausePartsOR.length > 1)
                extMultiClause(clauses, clausePartsOR, KeyWords.AND, subQuery);
            else
                extSingleClause(clauses, clearClauseString, subQuery);
        }
        whereAndHaving.add(new Query.WhereAndHavingClause(line, clauses, isHaving));
    }

    private static void extMultiClause(List<Query.Clause> clauses, String[] clauseParts, KeyWords keyWord, List<Query> subQuery) {
        String[] clausePartsDif;
        for (String andClause : clauseParts) {
            andClause = removeBrackets(andClause);
            clausePartsDif = andClause.split(keyWord.getWordInQuery());
            if (clausePartsDif.length > 1) {
                for (String orClause : clausePartsDif) {
                    extSingleClause(clauses, orClause, subQuery);
                }
            } else {
                extSingleClause(clauses, andClause, subQuery);
            }
        }
    }

    private static void extSingleClause(List<Query.Clause> clauses, String clause, List<Query> subQuery) {
        String[] clauseParts = clause.split(" ");

        for (String operator : operators) {
            if (clauseParts[1].contains(operator)) {
                String[] atr = clause.split(" " + operator + " ");
                if (atr[1].contains(KeyWords.SELECT.toString())) {
                    clauses.add(new Query.Clause(atr[0], operator, atr[1]));
                    String query = getCorrectQuery(atr[1]);
                    subQuery.add(parseThis(query));
                }
                else
                    clauses.add(new Query.Clause(atr[0], operator, atr[1]));
                break;
            }
        }
    }

    private static String removeKeyWord(String line, KeyWords keyWord) {
        return line.replaceFirst(keyWord.getWordInQuery(), "").replace(";", "");
    }

    private static String removeBrackets(String line) {
        Pattern pattern = Pattern.compile("^\\((.*)\\)$");
        Matcher match = pattern.matcher(line);

        return match.find() ? match.group(1) : line;
    }

    private static String removeBracketsAndAlias(String line) {
        Pattern pattern = Pattern.compile("^\\((.*)\\).*$");
        Matcher match = pattern.matcher(line);

        return match.find() ? match.group(1) : line;
    }

    private static void extJoins(List<Query.Join> joins, String line) {
        String joinType = line.substring(0, line.indexOf(" "));

        switch (joinType) {
            case "LEFT" -> line = removeKeyWord(removeKeyWord(line, KeyWords.LEFT), KeyWords.JOIN);
            case "RIGHT" -> line = removeKeyWord(removeKeyWord(line, KeyWords.RIGHT), KeyWords.JOIN);
            case "INNER" -> line = removeKeyWord(removeKeyWord(line, KeyWords.INNER), KeyWords.JOIN);
            default -> {
                line = removeKeyWord(line, KeyWords.JOIN);
                joinType = "";
            }
        }

        String[] joinParts = line.split(KeyWords.ON.getWordInQuery());
        Query.Join j = new Query.Join(joinType, joinParts[0], joinParts[1]);
        joins.add(j);
    }

    private static void extTwinExpr(List<Query.TwinExpr> twinExpr, String line, KeyWords keyWord) {
        line = removeKeyWord(line, keyWord);
        Query.TwinExpr extExpr;

        for (String expr: line.split(", ")) {
            String[] exprParts = expr.split(" ");

            if (exprParts.length > 1)
                extExpr = new Query.TwinExpr(exprParts[0], exprParts[1]);
            else
                extExpr = new Query.TwinExpr(expr);
            twinExpr.add(extExpr);
        }
    }

    private static String getCorrectQuery(String query) {
        KeyWords[] queryKeyWords = {KeyWords.FROM, KeyWords.WHERE, KeyWords.GROUP_BY, KeyWords.HAVING};
        query = removeBracketsAndAlias(query);
        for (KeyWords keyWord: queryKeyWords) {
            query = query.replace(keyWord.toString(), "\r" + keyWord);
        }
        return query;
    }


    private static void extSimple(List<String> simpleExpr, String line, KeyWords keyWord, List<Query> subQuery) {
        line = removeKeyWord(line, keyWord);
        String[] exprParts = line.split(", ");

        for (String expr : exprParts) {
            if (expr.contains(KeyWords.SELECT.toString())) {
                simpleExpr.add(expr);
                expr = getCorrectQuery(expr);
                subQuery.add(parseThis(expr));
            }
            else
                simpleExpr.add(expr);
        }
    }

    private static Integer extTruncations(String line, KeyWords keyWord) {
        return Integer.parseInt(removeKeyWord(line, keyWord));
    }
}
