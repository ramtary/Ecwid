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

    private class Join {
    }

    private class Source {
    }

    private class WhereClause {
    }

    private class Sort {
    }
}
