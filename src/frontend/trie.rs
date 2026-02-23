use super::token_type::TokenType;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct TrieNode {
    children: HashMap<char, TrieNode>,
    token_type: Option<TokenType>,
    is_keyword: bool,
}

impl TrieNode {
    fn new() -> Self {
        Self {
            children: HashMap::new(),
            token_type: None,
            is_keyword: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Trie {
    root: TrieNode,
}

impl Trie {
    pub fn new() -> Self {
        Self {
            root: TrieNode::new(),
        }
    }

    pub fn insert(&mut self, s: &str, token_type: TokenType) {
        let mut node = &mut self.root;
        for ch in s.chars() {
            node = node.children.entry(ch).or_insert_with(TrieNode::new);
        }
        node.token_type = Some(token_type.clone());
        node.is_keyword = token_type.is_keyword();
    }

    pub fn find_longest_match(&self, text: &str, start: usize) -> Option<(TokenType, usize)> {
        let mut node = &self.root;
        let mut matched_token = None;
        let mut matched_length = 0;

        for (i, ch) in text[start..].chars().enumerate() {
            if let Some(child) = node.children.get(&ch) {
                node = child;
                if let Some(ref token_type) = node.token_type {
                    matched_token = Some(token_type.clone());
                    matched_length = i + 1;
                }
            } else {
                break;
            }
        }

        matched_token.map(|t| (t, matched_length))
    }

    pub fn is_keyword(&self, s: &str) -> bool {
        let mut node = &self.root;
        for ch in s.chars() {
            match node.children.get(&ch) {
                Some(child) => node = child,
                None => return false,
            }
        }
        node.is_keyword
    }

    pub fn build_js_trie() -> Self {
        let mut trie = Self::new();

        let keywords = [
            ("break", TokenType::Break),
            ("case", TokenType::Case),
            ("catch", TokenType::Catch),
            ("class", TokenType::Class),
            ("const", TokenType::Const),
            ("continue", TokenType::Continue),
            ("debugger", TokenType::Debugger),
            ("default", TokenType::Default),
            ("delete", TokenType::Delete),
            ("do", TokenType::Do),
            ("else", TokenType::Else),
            ("export", TokenType::Export),
            ("extends", TokenType::Extends),
            ("finally", TokenType::Finally),
            ("for", TokenType::For),
            ("function", TokenType::Function),
            ("if", TokenType::If),
            ("import", TokenType::Import),
            ("in", TokenType::In),
            ("instanceof", TokenType::Instanceof),
            ("new", TokenType::New),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("switch", TokenType::Switch),
            ("this", TokenType::This),
            ("throw", TokenType::Throw),
            ("try", TokenType::Try),
            ("typeof", TokenType::Typeof),
            ("var", TokenType::Var),
            ("void", TokenType::Void),
            ("while", TokenType::While),
            ("with", TokenType::With),
            ("yield", TokenType::Yield),
            ("null", TokenType::Null),
            ("true", TokenType::True),
            ("false", TokenType::False),
        ];

        for (keyword, token_type) in keywords {
            trie.insert(keyword, token_type);
        }

        let operators = [
            ("===", TokenType::StrictEqual),
            ("!==", TokenType::StrictNotEqual),
            ("==", TokenType::Equal),
            ("!=", TokenType::NotEqual),
            ("<=", TokenType::LessEqual),
            (">=", TokenType::GreaterEqual),
            ("<<=", TokenType::LeftShiftAssign),
            (">>=", TokenType::RightShiftAssign),
            (">>>=", TokenType::UnsignedRightShiftAssign),
            ("**=", TokenType::ExponentAssign),
            ("&&", TokenType::LogicalAnd),
            ("||", TokenType::LogicalOr),
            ("??", TokenType::NullishCoalescing),
            ("<<", TokenType::LeftShift),
            (">>", TokenType::RightShift),
            (">>>", TokenType::UnsignedRightShift),
            ("++", TokenType::Increment),
            ("--", TokenType::Decrement),
            ("+=", TokenType::PlusAssign),
            ("-=", TokenType::MinusAssign),
            ("*=", TokenType::MultiplyAssign),
            ("/=", TokenType::DivideAssign),
            ("%=", TokenType::ModuloAssign),
            ("&=", TokenType::BitwiseAndAssign),
            ("|=", TokenType::BitwiseOrAssign),
            ("^=", TokenType::BitwiseXorAssign),
            ("=>", TokenType::Arrow),
            ("?.", TokenType::OptionalChaining),
            ("...", TokenType::Spread),
        ];

        for (op, token_type) in operators {
            trie.insert(op, token_type);
        }

        let single_ops = [
            ("+", TokenType::Plus),
            ("-", TokenType::Minus),
            ("*", TokenType::Multiply),
            ("/", TokenType::Divide),
            ("%", TokenType::Modulo),
            ("=", TokenType::Assign),
            ("<", TokenType::LessThan),
            (">", TokenType::GreaterThan),
            ("!", TokenType::LogicalNot),
            ("&", TokenType::BitwiseAnd),
            ("|", TokenType::BitwiseOr),
            ("^", TokenType::BitwiseXor),
            ("?", TokenType::Question),
            (":", TokenType::Colon),
        ];
        for (op, token_type) in single_ops {
            trie.insert(op, token_type);
        }

        trie
    }
}

impl Default for Trie {
    fn default() -> Self {
        Self::build_js_trie()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trie_keyword_insertion() {
        let mut trie = Trie::new();
        trie.insert("function", TokenType::Function);

        assert!(trie.is_keyword("function"));
        assert!(!trie.is_keyword("func"));
        assert!(!trie.is_keyword("functions"));
    }

    #[test]
    fn trie_longest_match() {
        let trie = Trie::build_js_trie();

        if let Some((token_type, length)) = trie.find_longest_match("=== 42", 0) {
            assert_eq!(token_type, TokenType::StrictEqual);
            assert_eq!(length, 3);
        } else {
            panic!("Should find strict equal");
        }

        if let Some((token_type, length)) = trie.find_longest_match("== 42", 0) {
            assert_eq!(token_type, TokenType::Equal);
            assert_eq!(length, 2);
        } else {
            panic!("Should find equal");
        }

        if let Some((token_type, length)) = trie.find_longest_match("&& true", 0) {
            assert_eq!(token_type, TokenType::LogicalAnd);
            assert_eq!(length, 2);
        } else {
            panic!("Should find logical AND");
        }

        if let Some((token_type, length)) = trie.find_longest_match("+ 1", 0) {
            assert_eq!(token_type, TokenType::Plus);
            assert_eq!(length, 1);
        } else {
            panic!("Should find plus");
        }
    }

    #[test]
    fn trie_no_match() {
        let trie = Trie::build_js_trie();
        assert!(trie.find_longest_match("hello", 0).is_none());
        assert!(trie.find_longest_match("fun", 0).is_none());
    }
}
