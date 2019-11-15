#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct NodeId(usize);

#[derive(Debug, Clone)]
enum Node<T> {
    Value(T),
    Refer(NodeId),
}

impl<T> Node<T> {
    fn new(t: T) -> Self {
        Node::Value(t)
    }

    fn refer(node_id: NodeId) -> Self {
        Node::Refer(node_id)
    }
}

#[derive(Debug)]
pub struct UnionFindPool<T> {
    pool: Vec<Node<T>>,
}

impl<T> UnionFindPool<T> {
    pub fn new() -> Self {
        Self { pool: vec![] }
    }

    fn register(&mut self, node: Node<T>) -> NodeId {
        self.pool.push(node);
        NodeId(self.pool.len() - 1)
    }

    pub fn node_new(&mut self, t: T) -> NodeId {
        self.register(Node::new(t))
    }

    pub fn node_refer(&mut self, id: NodeId) -> NodeId {
        self.register(Node::refer(id))
    }

    fn at(&self, node_id: NodeId) -> &Node<T> {
        &self.pool[node_id.0]
    }

    fn at_mut(&mut self, node_id: NodeId) -> &mut Node<T> {
        &mut self.pool[node_id.0]
    }

    pub fn value_of(&self, mut id: NodeId) -> &T {
        loop {
            match self.at(id) {
                Node::Value(t) => return t,
                Node::Refer(new_id) => id = *new_id,
            }
        }
    }

    fn new_origin(&mut self, mut start: NodeId, origin: NodeId) {
        loop {
            match self.at_mut(start) {
                node @ Node::Value(_) => {
                    *node = Node::refer(origin);
                    break;
                }
                Node::Refer(ref mut id) => {
                    start = *id;
                    *id = origin;
                }
            }
        }
    }

    pub fn unify_with(&mut self, id1: NodeId, id2: NodeId, unify: impl FnOnce(&T, &T) -> T) -> &T {
        let l = self.value_of(id1);
        let r = self.value_of(id2);
        let new = unify(l, r);
        let new_id = self.node_new(new);
        self.new_origin(id1, new_id);
        self.new_origin(id2, new_id);
        self.value_of(new_id)
    }
}
