/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */;

open TestFramework;
open Frame;

describe("Frame.Table", ({test}) => {
  test("Basic cell", ({expect}) => {
    let actual =
      Table.createElement(
        ~columns=[{width: 4}],
        ~children=[Row.createElement(~children=["Cell"], ())],
        (),
      );
    let expected = ["--------", "| Cell |", "--------"];
    expect.string(actual).toEqualLines(expected);
  });

  test("Basic table", ({expect}) => {
    let actual =
      Table.createElement(
        ~columns=[{width: 7}, {width: 4}],
        ~children=[
          Row.createElement(~children=["testing", "1234"], ()),
          Row.createElement(~children=["a", "b"], ()),
        ],
        (),
      );
    let expected = [
      "------------------",
      "| testing | 1234 |",
      "|---------+------|",
      "| a       | b    |",
      "------------------",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("mismatched first row", ({expect}) =>
    expect.string(
      Table.createElement(
        ~columns=[{width: 7}, {width: 3}],
        ~children=[
          Row.createElement(~children=["testing"], ()),
          Row.createElement(~children=["a", "b"], ()),
        ],
        (),
      ),
    ).
      toEqualLines([
      "-----------------",
      "| testing |     |",
      "|---------+-----|",
      "| a       | b   |",
      "-----------------",
    ])
  );

  test("mismatched last row", ({expect}) =>
    expect.string(
      Table.createElement(
        ~columns=[{width: 7}, {width: 4}],
        ~children=[
          Row.createElement(~children=["testing", "1234"], ()),
          Row.createElement(~children=["a"], ()),
        ],
        (),
      ),
    ).
      toEqualLines([
      "------------------",
      "| testing | 1234 |",
      "|---------+------|",
      "| a       |      |",
      "------------------",
    ])
  );

  test("Text wrapping", ({expect}) => {
    let actual =
      Table.createElement(
        ~columns=[{width: 7}, {width: 4}],
        ~children=[
          Row.createElement(~children=["test a long column", "this"], ()),
          Row.createElement(~children=["a", "b"], ()),
        ],
        (),
      );
    let expected = [
      "------------------",
      "| test a  | this |",
      "| long    |      |",
      "| column  |      |",
      "|---------+------|",
      "| a       | b    |",
      "------------------",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("Basic table JSX", ({expect}) => {
    let actual =
      <Table columns=[<ColumnConfig width=7 />, <ColumnConfig width=4 />]>
        <Row> "testing" "1234" </Row>
        <Row> "a" "b" </Row>
      </Table>;
    let expected = [
      "------------------",
      "| testing | 1234 |",
      "|---------+------|",
      "| a       | b    |",
      "------------------",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("Large table", ({expect}) => {
    let actual =
      <Table
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "------------------------------------------",
      "| a1       | b1               | c1       |",
      "|----------+------------------+----------|",
      "| a2       | b2 is a cell     | c2       |",
      "|          | that needs to    |          |",
      "|          | wrap             |          |",
      "|----------+------------------+----------|",
      "| a3       | b3               | c3       |",
      "|----------+------------------+----------|",
      "| a4       | b4               |          |",
      "|----------+------------------+----------|",
      "| a5       | b5               | c5       |",
      "------------------------------------------",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("Formatted Table", ({expect}) =>
    Pastel.useMode(
      HumanReadable,
      () => {
        let actual =
          <Table
            columns=[
              <ColumnConfig width=8 />,
              <ColumnConfig width=16 />,
              <ColumnConfig width=8 />,
            ]>
            <Row> "a1" <Pastel bold=true> "b1" </Pastel> "c1" </Row>
            <Row>
              "a2"
              <Pastel color=Blue> "b2 is a cell that needs to wrap" </Pastel>
              "c2"
            </Row>
            <Row> "a3" "b3" "c3" "d3" </Row>
            <Row> "a4" "b4" </Row>
            <Row> "a5" "b5" "c5" </Row>
          </Table>;
        let expected = [
          "------------------------------------------",
          "| a1       | <bold>b1              </resetDimAndBold> | c1       |",
          "|----------+------------------+----------|",
          "| a2       | <blue>b2 is a cell    </resetColor> | c2       |",
          "|          | <blue>that needs to   </resetColor> |          |",
          "|          | <blue>wrap            </resetColor> |          |",
          "|----------+------------------+----------|",
          "| a3       | b3               | c3       |",
          "|----------+------------------+----------|",
          "| a4       | b4               |          |",
          "|----------+------------------+----------|",
          "| a5       | b5               | c5       |",
          "------------------------------------------",
        ];
        expect.string(actual).toEqualLines(expected);
      },
    )
  );
});

describe("Table styles", ({test}) => {
  test("SimpleLines", ({expect}) => {
    let actual =
      <Table
        borderStyle=SimpleLines
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "------------------------------------------",
      "| a1       | b1               | c1       |",
      "|----------+------------------+----------|",
      "| a2       | b2 is a cell     | c2       |",
      "|          | that needs to    |          |",
      "|          | wrap             |          |",
      "|----------+------------------+----------|",
      "| a3       | b3               | c3       |",
      "|----------+------------------+----------|",
      "| a4       | b4               |          |",
      "|----------+------------------+----------|",
      "| a5       | b5               | c5       |",
      "------------------------------------------",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("BoxLight", ({expect}) => {
    let actual =
      <Table
        borderStyle=BoxLight
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "┌──────────┬──────────────────┬──────────┐",
      "│ a1       │ b1               │ c1       │",
      "├──────────┼──────────────────┼──────────┤",
      "│ a2       │ b2 is a cell     │ c2       │",
      "│          │ that needs to    │          │",
      "│          │ wrap             │          │",
      "├──────────┼──────────────────┼──────────┤",
      "│ a3       │ b3               │ c3       │",
      "├──────────┼──────────────────┼──────────┤",
      "│ a4       │ b4               │          │",
      "├──────────┼──────────────────┼──────────┤",
      "│ a5       │ b5               │ c5       │",
      "└──────────┴──────────────────┴──────────┘",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("None", ({expect}) => {
    let actual =
      <Table
        border=None
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "                                          ",
      "  a1         b1                 c1        ",
      "                                          ",
      "  a2         b2 is a cell       c2        ",
      "             that needs to                ",
      "             wrap                         ",
      "                                          ",
      "  a3         b3                 c3        ",
      "                                          ",
      "  a4         b4                           ",
      "                                          ",
      "  a5         b5                 c5        ",
      "                                          ",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("BoxLight with outer borders", ({expect}) => {
    let actual =
      <Table
        border=Outer
        borderStyle=BoxLight
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "┌────────────────────────────────────────┐",
      "│ a1         b1                 c1       │",
      "│                                        │",
      "│ a2         b2 is a cell       c2       │",
      "│            that needs to               │",
      "│            wrap                        │",
      "│                                        │",
      "│ a3         b3                 c3       │",
      "│                                        │",
      "│ a4         b4                          │",
      "│                                        │",
      "│ a5         b5                 c5       │",
      "└────────────────────────────────────────┘",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("BoxLight with inner borders", ({expect}) => {
    let actual =
      <Table
        border=Inner
        borderStyle=BoxLight
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "                                          ",
      "  a1       │ b1               │ c1        ",
      " ──────────┼──────────────────┼────────── ",
      "  a2       │ b2 is a cell     │ c2        ",
      "           │ that needs to    │           ",
      "           │ wrap             │           ",
      " ──────────┼──────────────────┼────────── ",
      "  a3       │ b3               │ c3        ",
      " ──────────┼──────────────────┼────────── ",
      "  a4       │ b4               │           ",
      " ──────────┼──────────────────┼────────── ",
      "  a5       │ b5               │ c5        ",
      "                                          ",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("SimpleLines with outer borders", ({expect}) => {
    let actual =
      <Table
        border=Outer
        borderStyle=SimpleLines
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "------------------------------------------",
      "| a1         b1                 c1       |",
      "|                                        |",
      "| a2         b2 is a cell       c2       |",
      "|            that needs to               |",
      "|            wrap                        |",
      "|                                        |",
      "| a3         b3                 c3       |",
      "|                                        |",
      "| a4         b4                          |",
      "|                                        |",
      "| a5         b5                 c5       |",
      "------------------------------------------",
    ];
    expect.string(actual).toEqualLines(expected);
  });

  test("SimpleLines with inner borders", ({expect}) => {
    let actual =
      <Table
        border=Inner
        borderStyle=SimpleLines
        columns=[
          <ColumnConfig width=8 />,
          <ColumnConfig width=16 />,
          <ColumnConfig width=8 />,
        ]>
        <Row> "a1" "b1" "c1" </Row>
        <Row> "a2" "b2 is a cell that needs to wrap" "c2" </Row>
        <Row> "a3" "b3" "c3" "d3" </Row>
        <Row> "a4" "b4" </Row>
        <Row> "a5" "b5" "c5" </Row>
      </Table>;
    let expected = [
      "                                          ",
      "  a1       | b1               | c1        ",
      " ----------+------------------+---------- ",
      "  a2       | b2 is a cell     | c2        ",
      "           | that needs to    |           ",
      "           | wrap             |           ",
      " ----------+------------------+---------- ",
      "  a3       | b3               | c3        ",
      " ----------+------------------+---------- ",
      "  a4       | b4               |           ",
      " ----------+------------------+---------- ",
      "  a5       | b5               | c5        ",
      "                                          ",
    ];
    expect.string(actual).toEqualLines(expected);
  });
});
