package com.ecwid.sqlp;

import java.util.List;

public class Query {
    private List<String> columns;
    private List<Source> fromSources;
    private List<Join> joins;
    private List<WhereClause> whereClauses;
    private List<String> groupByColumns;
    private List<Sort> sortColumns;
    private Integer limit;
    private Integer offset;

    public class Source {
        private String name;
        private String alias;
    }

    public class Join {
        private String joinType;
        private String joinTable;
        private String onFields;
    }

    public class WhereClause {
        private String clause;
        private Boolean isHaving;
    }

    public class Sort {
        private String sortType;
        private String sortField;
    }
}
