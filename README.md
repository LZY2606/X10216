# 类型切片台 (Type Slicer)

Hindley-Milner 风格小型语言的类型推断可视化工作台：解析 let / 多态 / 函数 /
record / variant，生成带来源范围的约束图，逐步执行实例化、泛化与统一。

## 运行

```sh
cargo build --all-targets
cargo test --all-targets
cargo run -- --addr 127.0.0.1:5276
# 打开 http://127.0.0.1:5276
```

可选参数：`--db <path>`（默认 `type-slicer.db`，SQLite，会话重启后可恢复）。

## 语言

```text
let id = fun x -> x in            -- let / 多态
let rec f = fun n -> f n in f     -- 递归绑定
{ a = 1, b = true }.a             -- record 与投影（行多态）
match `Some 1 with | `Some x -> x | `None y -> 0   -- variant
if true then 1 else 0
```

## 特性

- **约束图与来源**：每条约束携带表达式 span 与描述；每个类型可追溯到产生它的约束。
- **求解器**：occurs-check（无限类型）、value restriction（仅泛化语法值）、
  行多态区分「缺字段」(MissingFields) 与「多字段」(ExtraFields)。
- **错误核心**：对每个错误计算保持不可满足的极小核心；存在多个同样小的
  核心时全部保留；可临时禁用任一约束查看后果（`/api/versions/:id/disable`）。
- **增量**：约束 id = 子树结构键（alpha 不变，de Bruijn）+ 作用域 + 泛化环境；
  变量改名不会使全部身份漂移；版本对比显示复用 / 失效 / 重新求解。
- **持久化**：会话与每个版本的完整分析存入 SQLite，重启后可从任一步骤继续
  （`/api/versions/:id/steps?from=k`）。

## API

- `POST /api/sessions` 新会话
- `POST /api/sessions/:id/analyze` `{source}` 分析并存为新版本
- `GET  /api/sessions/:id` 版本列表
- `GET  /api/versions/:id` 恢复某版本的完整分析
- `GET  /api/versions/:id/steps?from=&to=` 步骤切片
- `GET  /api/versions/:id/diff?against=:id2` 约束复用对比
- `POST /api/versions/:id/disable` `{constraint_id}` 禁用约束后重新求解
