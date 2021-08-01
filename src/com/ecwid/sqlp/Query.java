package com.ecwid.sqlp;

import java.util.List;

public class Query {
    private final List<String> columns;
    private final List<TwinExpr> fromSources;
    private final List<Join> joins;
    private final List<WhereAndHavingClause> whereAndHavingClauses;
    private final List<String> groupByColumns;
    private final List<TwinExpr> sortColumns;
    private final Integer limit;
    private final Integer offset;
    private final List<Query> subQuery;

    public Query(List<String> columns, List<TwinExpr> fromSources, List<Join> joins,
                 List<WhereAndHavingClause> whereAndHavingClauses, List<String> groupByColumns,
                 List<TwinExpr> sortColumns, Integer limit, Integer offset, List<Query> subQuery) {
        this.columns = columns;
        this.fromSources = fromSources;
        this.joins = joins;
        this.whereAndHavingClauses = whereAndHavingClauses;
        this.groupByColumns = groupByColumns;
        this.sortColumns = sortColumns;
        this.limit = limit;
        this.offset = offset;
        this.subQuery = subQuery;
    }

    @Override
    public String toString() {
        String sources = "";
        for (TwinExpr twinExpr: fromSources) {
            sources = twinExpr.toString("Источник: " , "Алиас: ");
        }

        String sorts = "";
        for (TwinExpr twinExpr: sortColumns) {
            sorts = twinExpr.toString("Поле: " , "Порядок: ");
        }

        String join = "";
        for (Join expr: joins) {
            join = expr.toString();
        }

        String whereAndHaving = "";
        for (WhereAndHavingClause expr: whereAndHavingClauses) {
            whereAndHaving = expr.toString();
        }

        return "\n" + "Поля: " + columns + "\n\n" +
                "Источники: " + sources + "\n\n" +
                "Объединения: " + join + "\n\n" +
                "Фильтрующие условия: " + whereAndHaving + "\n\n" +
                "Группировки: " + groupByColumns + "\n\n" +
                "Сортировки: " + sorts + "\n\n" +
                "Limit: " + limit + "\n\n" +
                "Offset: " + offset + "\n\n" +
                "Подзапросы: " + subQuery + "\n\n";
    }

    public static class TwinExpr {
        private final String partOne;
        private final String partTwo;

        public TwinExpr(String partOne, String partTwo) {
            this.partOne = partOne;
            this.partTwo = partTwo;
        }

        public TwinExpr(String partOne) {
            this.partOne = partOne;
            this.partTwo = "";
        }

        public String toString(String namePartOne, String namePartTwo) {
            return "\n" + namePartOne + partOne + "\n"
                    + namePartTwo + partTwo;
        }
    }

    public record Join(String joinType, String joinTable, String onFields) {

        @Override
        public String toString() {
            return "\n" + "Тип: " + joinType + "\n"
                    + "Таблица/предстваление: " + joinTable + "\n"
                    + "По полям: " + onFields;
        }
    }

    public record WhereAndHavingClause(String fullClause, List<Clause> clauses,
                                       boolean isHaving) {

        @Override
        public String toString() {
            StringBuilder clausesString = new StringBuilder();
            for (Clause clause : clauses)
                clausesString.append("\n").append("Условие: ").append(clause.toString());

            return "\n" + "Полное условие: " + fullClause + clausesString + "\n" + "Это Having: " + isHaving;
        }
    }

    public record Clause(String field, String operator, String value) {

        @Override
        public String toString() {
            return "\n" + "Поле: " + field + "\n"
                    + "Оператор: " + operator + "\n"
                    + "Значение: " + value;
        }
    }

}
