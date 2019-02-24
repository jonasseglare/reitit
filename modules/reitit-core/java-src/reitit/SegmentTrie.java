package reitit;

import clojure.lang.Keyword;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.*;

public class SegmentTrie {

  public static ArrayList<String> split(final String path) {
    final ArrayList<String> segments = new ArrayList<>(4);
    final int size = path.length();
    int start = 1;
    for (int i = start; i < size; i++) {
      final char c = path.charAt(i);
      if (c == '/') {
        segments.add(path.substring(start, i));
        start = i + 1;
      }
    }
    if (start <= size) {
      segments.add(path.substring(start, size));
    }
    return segments;
  }

  private static String decode(String s) {
    try {
      if (s.contains("%")) {
        String _s = s;
        if (s.contains("+")) {
          _s = s.replace("+", "%2B");
        }
        return URLDecoder.decode(_s, "UTF-8");
      }
    } catch (UnsupportedEncodingException ignored) {
    }
    return s;
  }

  public static class Match {
    public final Map<Keyword, String> params = new HashMap<>();
    public Object data;

    @Override
    public String toString() {
      Map<Object, Object> m = new HashMap<>();
      m.put(Keyword.intern("data"), data);
      m.put(Keyword.intern("params"), params);
      return m.toString();
    }
  }

  private Map<String, SegmentTrie> childs = new HashMap<>();
  private Map<Keyword, SegmentTrie> wilds = new HashMap<>();
  private Map<Keyword, SegmentTrie> catchAll = new HashMap<>();
  private Object data;

  public SegmentTrie add(String path, Object data) {
    List<String> paths = split(path);
    SegmentTrie pointer = this;
    for (String p : paths) {
      if (p.startsWith(":")) {
        Keyword k = Keyword.intern(p.substring(1));
        SegmentTrie s = pointer.wilds.get(k);
        if (s == null) {
          s = new SegmentTrie();
          pointer.wilds.put(k, s);
        }
        pointer = s;
      } else if (p.startsWith("*")) {
        Keyword k = Keyword.intern(p.substring(1));
        SegmentTrie s = pointer.catchAll.get(k);
        if (s == null) {
          s = new SegmentTrie();
          pointer.catchAll.put(k, s);
        }
        pointer = s;
        break;
      } else {
        SegmentTrie s = pointer.childs.get(p);
        if (s == null) {
          s = new SegmentTrie();
          pointer.childs.put(p, s);
        }
        pointer = s;
      }
    }
    pointer.data = data;
    return this;
  }

  private Matcher staticMatcher() {
    if (childs.size() == 1) {
      return new StaticMatcher(childs.keySet().iterator().next(), childs.values().iterator().next().matcher());
    } else {
      Map<String, Matcher> m = new HashMap<>();
      for (Map.Entry<String, SegmentTrie> e : childs.entrySet()) {
        m.put(e.getKey(), e.getValue().matcher());
      }
      return new StaticMapMatcher(m);
    }
  }

  public Matcher matcher() {
    Matcher m;
    if (!catchAll.isEmpty()) {
        // Whenever there are catchAll tries,
        // other matchers are ignored.
      m = new CatchAllMatcher(catchAll.keySet().iterator().next(), catchAll.values().iterator().next().data);

      // The DataMatcher simply decorates the result with some data.
      if (data != null) {
        m = new LinearMatcher(Arrays.asList(new DataMatcher(data), m));
      }
    } else if (!wilds.isEmpty()) {
        // 
      if (wilds.size() == 1 && data == null && childs.isEmpty()) {
        m = new WildMatcher(wilds.keySet().iterator().next(), wilds.values().iterator().next().matcher());
      } else {
        List<Matcher> matchers = new ArrayList<>();

        // In case this is the last in the chain, attach data
        if (data != null) {
          matchers.add(new DataMatcher(data));
        }
        
        // Prioritize static matchers...
        if (!childs.isEmpty()) {
          matchers.add(staticMatcher());
        }

        // ... but if no static matcher matches, consider 
        // the wild ones.
        for (Map.Entry<Keyword, SegmentTrie> e : wilds.entrySet()) {
          matchers.add(new WildMatcher(e.getKey(), e.getValue().matcher()));
        }
        m = new LinearMatcher(matchers);
      }
    } else if (!childs.isEmpty()) {
      m = staticMatcher();
      if (data != null) {
        m = new LinearMatcher(Arrays.asList(new DataMatcher(data), m));
      }
    } else {
      return new DataMatcher(data);
    }
    return m;
  }

  public interface Matcher {
    Match match(int i, List<String> segments, Match match);
  }

  public static final class StaticMatcher implements Matcher {
    private final String segment;
    private final Matcher child;

    StaticMatcher(String segment, Matcher child) {
      this.segment = segment;
      this.child = child;
    }

    @Override
    public Match match(int i, List<String> segments, Match match) {
      if (i < segments.size() && segment.equals(segments.get(i))) {
        return child.match(i + 1, segments, match);
      }
      return null;
    }

    @Override
    public String toString() {
      return "[\"" + segment + "\" " + child + "]";
    }
  }

  public static final class WildMatcher implements Matcher {
    private final Keyword parameter;
    private final Matcher child;

    WildMatcher(Keyword parameter, Matcher child) {
      this.parameter = parameter;
      this.child = child;
    }

    @Override
    public Match match(int i, List<String> segments, Match match) {
      if (i < segments.size() && !segments.get(i).isEmpty()) {
        final Match m = child.match(i + 1, segments, match);
        if (m != null) {
          m.params.put(parameter, decode(segments.get(i)));
          return m;
        }
      }
      return null;
    }

    @Override
    public String toString() {
      return "[" + parameter + " " + child + "]";
    }
  }

  public static final class CatchAllMatcher implements Matcher {
    private final Keyword parameter;
    private final Object data;

    CatchAllMatcher(Keyword parameter, Object data) {
      this.parameter = parameter;
      this.data = data;
    }

    @Override
    public Match match(int i, List<String> segments, Match match) {
      if (i < segments.size()) {
        match.params.put(parameter, decode(String.join("/", segments.subList(i, segments.size()))));
        match.data = data;
        return match;
      }
      return null;
    }

    @Override
    public String toString() {
      return "[" + parameter + " " + new DataMatcher(data) + "]";
    }
  }

  public static final class StaticMapMatcher implements Matcher {
    private final Map<String, Matcher> map;

    StaticMapMatcher(Map<String, Matcher> map) {
      this.map = map;
    }

    @Override
    public Match match(int i, List<String> segments, Match match) {
      if (i < segments.size()) {
        final Matcher child = map.get(segments.get(i));
        if (child != null) {
          return child.match(i + 1, segments, match);
        }
      }
      return null;
    }

    @Override
    public String toString() {
      StringBuilder b = new StringBuilder();
      b.append("{");
      List<String> keys = new ArrayList<>(map.keySet());
      for (int i = 0; i < keys.size(); i++) {
        String path = keys.get(i);
        Matcher value = map.get(path);
        b.append("\"").append(path).append("\" ").append(value);
        if (i < keys.size() - 1) {
          b.append(", ");
        }
      }
      b.append("}");
      return b.toString();
    }
  }

  public static final class LinearMatcher implements Matcher {

    private final List<Matcher> childs;

    LinearMatcher(List<Matcher> childs) {
      this.childs = childs;
    }

    @Override
    public Match match(int i, List<String> segments, Match match) {
      for (Matcher child : childs) {
        final Match m = child.match(i, segments, match);
        if (m != null) {
          return m;
        }
      }
      return null;
    }

    @Override
    public String toString() {
      return childs.toString();
    }
  }

  public static final class DataMatcher implements Matcher {
    private final Object data;

    DataMatcher(Object data) {
      this.data = data;
    }

    @Override
    public Match match(int i, List<String> segments, Match match) {
      if (i == segments.size()) {
        match.data = data;
        return match;
      }
      return null;
    }

    @Override
    public String toString() {
      return (data != null ? data.toString() : "nil");
    }
  }

  public static Matcher scanner(List<Matcher> matchers) {
    return new LinearMatcher(matchers);
  }

  public static Match lookup(Matcher matcher, String path) {
    return matcher.match(0, split(path), new Match());
  }

  public static void main(String[] args) {

    SegmentTrie trie = new SegmentTrie();
    trie.add("/repos/:owner/:repo/stargazers", 1);
    Matcher m = trie.matcher();
    System.err.println(m);
    System.err.println(m.getClass());
    System.out.println(lookup(m, "/repos/metosin/reitit/stargazers"));
  }
}
