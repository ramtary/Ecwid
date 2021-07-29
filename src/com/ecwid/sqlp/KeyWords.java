package com.ecwid.sqlp;

public enum KeyWords {
    SELECT ("SELECT"),
    FROM ("FROM"),
    JOIN ("JOIN"),
    LEFT ("LEFT"),
    RIGHT ("RIGHT"),
    INNER ("INNER"),
    ON ("ON"),
    WHERE ("WHERE"),
    HAVING ("HAVING"),
    AND ("AND"),
    OR ("OR"),
    GROUP_BY ("GROUP BY"),
    ORDER_BY ("ORDER BY"),
    LIMIT ("LIMIT"),
    OFFSET ("OFFSET");

    private final String word;

    KeyWords(String word) {
        this.word = word;
    }

    public String getWordInQuery() {
        return (this == AND | this == OR | this == ON) ? " " + this.word + " " : this.word + " ";
    }

    @Override
    public String toString() {
        return this.word;
    }
}
