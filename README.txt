Парсер SQL запросов:
- Перечисление полей выборки явно (с алиасами) или *
- Неявное объединение нескольких таблиц (select * from A,B,C)
- Явное объединение таблиц (inner, left, right, full join)
- Фильтрующие условия (where a = 1 and b > 100)
- Подзапросы (select * from (select * from A) a_alias)
- Группировка по одному или нескольким полям (group by)
- Сортировка по одному или нескольким полям (order by)
- Усечение выборки (limit, offset)


Класс я чуть чуть изменил, добавив подзапросы и объединив Where и Having.

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
}

Тестовый запрос без особого смысла, просто для теста.