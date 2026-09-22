// Static term descriptors attached to generator-produced concrete nodes,
// and quantified schemes produced at generalized lets.

use crate::constraints::NodeIdT;
use crate::types::FlexKind;

#[derive(Clone, Debug, PartialEq)]
pub enum BaseS {
    Con(String),
    Arrow(NodeIdT, NodeIdT),
    /// fields in source order; tail Closed or a row variable node.
    Row {
        sort: RowSortS,
        fields: Vec<(String, NodeIdT)>,
        tail: TailS,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowSortS {
    Record,
    Variant,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TailS {
    Closed,
    Open(NodeIdT),
}

#[derive(Clone, Debug)]
pub struct FlexDesc {
    pub kind: FlexKind,
    pub rank: u32,
    pub hint: String,
}

#[derive(Clone, Debug)]
pub struct Scheme {
    /// Flex vars generalized.
    pub quantified: Vec<NodeIdT>,
    /// Any root reference of the value's type; descriptors remain in solver
    /// tables and instantiation copies the term graph.
    pub body: NodeIdT,
    pub origin_let: crate::constraints::CId,
}
