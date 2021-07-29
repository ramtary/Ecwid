package com.ecwid.sqlp;

import java.util.List;

public class Query {
    private List<String> columns;
    private List<Source> fromSources;
    private List<Join> joins;
    private List<WhereAndHavingClause> whereAndHavingClauses;
    private List<String> groupByColumns;
    private List<Sort> sortColumns;
    private Integer limit;
    private Integer offset;

    public Query(List<String> columns, List<Source> fromSources, List<Join> joins, List<WhereAndHavingClause> whereAndHavingClauses, List<String> groupByColumns, List<Sort> sortColumns, Integer limit, Integer offset) {
        this.columns = columns;
        this.fromSources = fromSources;
        this.joins = joins;
        this.whereAndHavingClauses = whereAndHavingClauses;
        this.groupByColumns = groupByColumns;
        this.sortColumns = sortColumns;
        this.limit = limit;
        this.offset = offset;
    }

    public List<String> getColumns() {
        return columns;
    }

    public void setColumns(List<String> columns) {
        this.columns = columns;
    }

    public List<Source> getFromSources() {
        return fromSources;
    }

    public void setFromSources(List<Source> fromSources) {
        this.fromSources = fromSources;
    }

    public List<Join> getJoins() {
        return joins;
    }

    public void setJoins(List<Join> joins) {
        this.joins = joins;
    }

    public List<WhereAndHavingClause> getWhereAndHavingClauses() {
        return whereAndHavingClauses;
    }

    public void setWhereAndHavingClauses(List<WhereAndHavingClause> whereAndHavingClauses) {
        this.whereAndHavingClauses = whereAndHavingClauses;
    }

    public List<String> getGroupByColumns() {
        return groupByColumns;
    }

    public void setGroupByColumns(List<String> groupByColumns) {
        this.groupByColumns = groupByColumns;
    }

    public List<Sort> getSortColumns() {
        return sortColumns;
    }

    public void setSortColumns(List<Sort> sortColumns) {
        this.sortColumns = sortColumns;
    }

    public Integer getLimit() {
        return limit;
    }

    public void setLimit(Integer limit) {
        this.limit = limit;
    }

    public Integer getOffset() {
        return offset;
    }

    public void setOffset(Integer offset) {
        this.offset = offset;
    }

    public static class Source {
        private String name;
        private String alias;

        public Source(String name, String alias) {
            this.name = name;
            this.alias = alias;
        }

        public Source(String name) {
            this.name = name;
            this.alias = "";
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getAlias() {
            return alias;
        }

        public void setAlias(String alias) {
            this.alias = alias;
        }
    }

    public static class Join {
        private String joinType;
        private String joinTable;
        private String onFields;

        public Join(String joinType, String joinTable, String onFields) {
            this.joinType = joinType;
            this.joinTable = joinTable;
            this.onFields = onFields;
        }

        public String getJoinType() {
            return joinType;
        }

        public void setJoinType(String joinType) {
            this.joinType = joinType;
        }

        public String getJoinTable() {
            return joinTable;
        }

        public void setJoinTable(String joinTable) {
            this.joinTable = joinTable;
        }

        public String getOnFields() {
            return onFields;
        }

        public void setOnFields(String onFields) {
            this.onFields = onFields;
        }
    }

    public static class WhereAndHavingClause {
        private String fullClause;
        private List<Clause> clauses;
        private boolean isHaving;

        public WhereAndHavingClause(String fullClause, List<Clause> clauses, boolean isHaving) {
            this.fullClause = fullClause;
            this.clauses = clauses;
            this.isHaving = isHaving;
        }

        public String getFullClause() {
            return fullClause;
        }

        public void setFullClause(String fullClause) {
            this.fullClause = fullClause;
        }

        public List<Clause> getClauses() {
            return clauses;
        }

        public void setClauses(List<Clause> clauses) {
            this.clauses = clauses;
        }

        public Boolean getHaving() {
            return isHaving;
        }

        public void setHaving(Boolean having) {
            isHaving = having;
        }
    }

    public static class Clause {
        private String field;
        private String operator;
        private String value;

        public Clause(String field, String operator, String value) {
            this.field = field;
            this.operator = operator;
            this.value = value;
        }

        public String getField() {
            return field;
        }

        public void setField(String field) {
            this.field = field;
        }

        public String getOperator() {
            return operator;
        }

        public void setOperator(String operator) {
            this.operator = operator;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

    public static class Sort {
        private String sortType;
        private String sortField;

        public Sort(String sortType, String sortField) {
            this.sortType = sortType;
            this.sortField = sortField;
        }

        public Sort(String sortField) {
            this.sortType = "";
            this.sortField = sortField;
        }

        public String getSortType() {
            return sortType;
        }

        public void setSortType(String sortType) {
            this.sortType = sortType;
        }

        public String getSortField() {
            return sortField;
        }

        public void setSortField(String sortField) {
            this.sortField = sortField;
        }
    }
}
